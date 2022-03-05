namespace FsCgp
open CgpBase
open System

type Indv<'a> = {Genome:Genome<'a>; mutable Loss : float} //fitter individual has lower loss
type TestCases<'a> = ('a[]*'a[])[]
type Evaluator<'a> = TestCases<'a>->Genome<'a>->float
type Evalconcurrency = Parallel | Basic
type Verbosity = Silent | Verbose

type EvaluatorSpec<'a> = 
    | Default 
    | Dropout of dropPct:float * resampleAfter:int

///generation -> loss history -> bool (true to terminate run)
type Terminator = int->float list->bool 

module CgpRun =
  open System.Threading
  open System.IO
  //open Microsoft.CSharp

  ///function to convert float to string to be used as part of caching key
  let floatCache (f:float) = String.Format("{0:0.##################}",f)

  ///function to convert int to string to be used as part of caching key
  let intCache (f:int) = string f

  let writeIndv ident indv (sw:StreamWriter) = 
    let ids = new String([|for _ in 1 .. ident -> ' '|])
    sw.Write ids 
    sw.WriteLine "{Genome = "
    writeGenome (ident+10) indv.Genome sw
    sw.Write ids
    sw.WriteLine (sprintf " Loss = %f" indv.Loss)
    sw.Write ids
    sw.WriteLine "}"
    
  let dumpIndv indv =
    printfn "{Genome = "
    dumpGenomeI indv.Genome 10
    printfn  " Loss = %f" indv.Loss
    printfn "}"

  let genPop (cspec:CompiledSpec<'a>) sz = [for i in 0..sz-1 -> {Genome=randomGenome cspec; Loss=System.Double.MaxValue}]

  let copyIndv i = {i with Genome=copyGenome i.Genome; Loss=i.Loss}

  module private  Evaluation =

      ///a basic evaluator 
      ///where the test_cases are a seq of tuples of (input vector, output vector)
      ///loss_func takes genome output and actual ouput and returns calculated loss
      let defaultEvaluator<'a> cspec (loss_func:'a[]->'a[]->float) =
        fun test_cases genome ->
            let ev = evaluator cspec genome
            let fit =
              test_cases
              |> Seq.averageBy(fun (x,y) -> 
                let y' = ev x
                let caseLoss = loss_func y y'
                caseLoss)
            if System.Double.IsNaN fit then System.Double.MaxValue else fit

      ///Wrapper type for parallel evaluation
      type PEvalulator<'a>(cspec:CompiledSpec<'a>, genome) =
        let ev =  evaluator cspec genome
        member x.Evaluator = ev
  
      //evaluate test cases in parallel
      let defaultEvaluatorPar<'a> cspec (loss_func:'a[]->'a[]->float) =
        fun test_cases genome ->
            let ev = new ThreadLocal<PEvalulator<'a>>(fun _ -> PEvalulator(cspec,genome))
            let fit =
              test_cases 
              |> Array.Parallel.map (fun (x,y)->
                let y' = ev.Value.Evaluator x
                let caseLoss = loss_func y y'
                caseLoss)
              |> Array.sum
            if System.Double.IsNaN fit then System.Double.MaxValue else fit


      ///generate key for caching
      ///two functionally equivalent genomes should generate the same key
      let genKey constKeyGen cspec genome =
        let mask = genomeMask cspec genome
        let activeNodes = Seq.zip mask genome.G |> Seq.map(fun (b,n) -> (if b then string n else "-" ) |> box)
        let consts = genome.Constants |> Seq.map (fun c-> (constKeyGen c) :> obj)
        let parms = Seq.append activeNodes consts
        let k = String.Join("_",Seq.toArray parms)
        k

      let dropoutEvaluator<'a> (baseEvaluator:Evaluator<'a>) dropPct resampleAfter =
        let sample = ref [||]
        let count = ref 0                 //resampling  at every evalution is unstable (resampleAter should be twice the population size)
        fun test_cases genome ->
            if count.Value = 0 then
                sample.Value <- test_cases |> Probability.Seq.sample (1.0 - dropPct) |> Seq.toArray
            count.Value <- count.Value + 1
            if count.Value > resampleAfter then count.Value <- 0
            baseEvaluator test_cases genome

  let createEvaluator<'a> cspec loss evalConcurrency evalSpec : (('a[]*'a[])[] -> Genome<'a>->float) = 
    let baseEvaluator = 
        match evalConcurrency with 
        | Parallel -> Evaluation.defaultEvaluatorPar<'a> cspec loss 
        | Basic -> Evaluation.defaultEvaluator<'a> cspec loss
    match evalSpec with
    | Default -> baseEvaluator 
    | Dropout (d,c) -> Evaluation.dropoutEvaluator<'a> baseEvaluator d c

  let runParent cspec lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) parent =
    let children = 
        [for i in 1 .. lambda do 
            let child = copyIndv parent
            mutate cspec child.Genome
            child.Loss <- evaluator test_cases child.Genome
            yield child
        ]
    let bestChild = children |> List.minBy (fun x->x.Loss)
    let parent' = if bestChild.Loss < parent.Loss then bestChild else parent
    parent'
        
  ///'speciated' mu plus lambda - seems to perform better
  let runGenMuSpeciated cspec parents mu lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) =
    let explrtryPop = genPop cspec 2 //exploratory genomes
    explrtryPop |> List.iter (fun i -> i.Loss<-evaluator test_cases i.Genome)
    let bestExpIndv = explrtryPop |> List.minBy (fun i->i.Loss)
    let children = parents |> List.map (runParent cspec lambda evaluator test_cases)
    let orderedIndvs = children @ [bestExpIndv] |> List.sortBy (fun i -> i.Loss)
    orderedIndvs |> List.truncate mu

  ///traditional mu + lambda 
  let runGenMu cspec parents mu lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) =
    let explrtryPop = genPop cspec 2 //exploratory genomes
    let children = (parents |> List.collect (fun p -> [for i in 1 .. lambda -> copyIndv p])) 
    children |> List.iter (fun p -> mutate cspec p.Genome) 
    Array.iter(fun p -> p.Loss <- evaluator test_cases p.Genome) (List.toArray (children @ explrtryPop))
    let orderedIndvs = Seq.append parents children |> Seq.sortBy (fun i -> i.Loss)
    orderedIndvs |> Seq.truncate mu |> Seq.toList
        
  ///mu parents + lambda mutated individuals method
  ///each generation, the best mu individuals are picked as parents

  let runMuPlusLambda 
    verbosity 
    cspec
    mu
    lambda
    (evaluator:Evaluator<_>)
    (test_cases:TestCases<_>)
    (terminator:Terminator) 
    ///callback to get new best individual when found
    postNewBest 
    startIndv =
    let parents = 
        startIndv 
        |> Option.map(fun i -> genPop cspec (mu-1) @ [i])
        |> Option.defaultValue (genPop cspec mu)

    let rec loop i hist parents =
        if not(List.isEmpty hist) && terminator i hist then 
            match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
        else
            let parents' = runGenMuSpeciated cspec parents mu lambda evaluator test_cases
            if parents'.[0].Loss < parents.[0].Loss then
                match verbosity with Verbose -> printfn "new best %.10f" parents'.[0].Loss | _ -> ()
                postNewBest parents'.[0]
            let hist' = ((parents' |> List.map (fun x->x.Loss)) @ hist) |> List.truncate 5
            loop (i+1) hist' parents'
    loop 1 [] parents

  ///one parent + lambda random individuals method
  ///each generation, the best individual is picked as parent
  ///only parent is mutated the rest are randomly generated
  ///currentBest is updated as new best indvidual is found
  //this could be a long running process so intermediate bests are useful for 
  //visualization and saving
  let run1PlusLambda 
    verbosity 
    cspec
    lambda
    (evaluator:Evaluator<_>)
    (test_cases:TestCases<_>)
    (terminator:Terminator) 
    ///callback to get new best individual when found
    postNewBest 
    startIndv
    =
    runMuPlusLambda verbosity cspec 1 lambda evaluator test_cases terminator postNewBest startIndv

  ///see run1PlusLambda for details
  ///dynamic version where the environment (test_cases) can change
  ///at any time invalidating existing losses
  let inline run1PlusLambdaDynamic 
    verbosity 
    cspec
    lambda
    (evaluator:Evaluator<_>)
    (testStream:IObservable<TestCases<_>>)
    (terminator:Terminator) 
    ///callback to get new best individual when found
    postNewBest 
    startIndv
    =
    let change = ref false;
    let current = ref [||]
    let _  = 
        testStream.Subscribe(fun test_cases ->
            current.Value <- test_cases
            change.Value <- true
            match verbosity with Verbose -> printfn "test cases changed"  | _ -> ()
            )

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
      else
        let parent =
            if change.Value then
                let p = copyIndv parent
                p.Loss <- evaluator current.Value parent.Genome //update parent loss if test cases changed
                change.Value <- false
                p
            else
                parent
        let parent' = runGenMuSpeciated cspec [parent] 1 lambda evaluator current.Value |> List.head
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          postNewBest parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    let initIndv = startIndv |> Option.defaultValue {Genome=randomGenome cspec; Loss=System.Double.MaxValue}
    loop 1 [] initIndv
