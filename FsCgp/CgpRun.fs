namespace FsCgp
open CgpBase
open System
open System.Runtime.Caching

type Indv<'a> = {Genome:Genome<'a>; mutable Loss : float} //fitter individual has lower loss
type TestCases<'a> = ('a[]*'a[])[]
type Evaluator<'a> = TestCases<'a>->Genome<'a>->float
type Evalconcurrency = Parallel | Basic
type CacheSpec<'a> = {Cache:MemoryCache; Cspec:CompiledSpec<'a>; ConstGen:'a->string}
type Verbosity = Silent | Verbose

type EvaluatorSpec<'a> = 
    | Default 
    | Cached of CacheSpec<'a> 
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

  let genPop (cspec:CompiledSpec<'a>) sz = [|for i in 0..sz-1 -> {Genome=randomGenome cspec; Loss=System.Double.MaxValue} |]

  let copyIndv i = {i with Genome=copyGenome i.Genome; Loss=i.Loss}

  module private  Evaluation =

      ///a basic evaluator 
      ///where the test_cases are a seq of tuples of (input vector, output vector)
      ///loss_func takes genome output and actual ouput and returns calculated loss
      let defaultEvaluator<'a> cspec (loss_func:'a[]->'a[]->float) =
        fun test_cases genome ->
            let ev = evaluator cspec genome
            let fit =
              (0.0, test_cases) 
              ||> Seq.fold(fun accLoss (x,y) -> 
                let y' = ev x
                let caseLoss = loss_func y y'
                accLoss + caseLoss
              )
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

      let cachedEvaluator<'a> (baseEvaluator:Evaluator<'a>) (cacheSpec:CacheSpec<'a>) =
        fun test_cases genome ->
            let key = genKey cacheSpec.ConstGen cacheSpec.Cspec genome
            let ci = cacheSpec.Cache.Item key
            let loss =
              if ci <> null then
                let cacheLoss = ci :?> float
                (*
                //validation
                let cacheIndv = ci :?> Indv<'a>
                let cacheLoss = cacheIndv.Loss
                let eLoss = evaluator indv.Genome
                if eLoss <> cacheLoss then 
                  let mC = genomeMask cspec cacheIndv.Genome
                  let mI = genomeMask cspec indv.Genome
                  dumpGenome cacheIndv.Genome
                  dumpGenome indv.Genome 
                  failwithf "loss mismatc %f<>%f" eLoss cacheLoss
                  *)
                cacheLoss
              else
                baseEvaluator test_cases genome
            if ci = null then
                cacheSpec.Cache.Add(key,loss,DateTimeOffset.Now.AddHours(2.0)) |> ignore
            loss

      let dropoutEvaluator<'a> (baseEvaluator:Evaluator<'a>) dropPct resampleAfter =
        let sample = ref [||]
        let count = ref 0                 //resampling  at every evalution is unstable (resampleAter should be twice the population size)
        fun test_cases genome ->
            if !count = 0 then
                sample := test_cases |> Probability.Seq.sample (1.0 - dropPct) |> Seq.toArray
            count := !count + 1
            if !count > resampleAfter then count := 0
            baseEvaluator test_cases genome

  let createEvaluator<'a> cspec loss evalConcurrency evalSpec : (('a[]*'a[])[] -> Genome<'a>->float) = 
    let baseEvaluator = 
        match evalConcurrency with 
        | Parallel -> Evaluation.defaultEvaluatorPar<'a> cspec loss 
        | Basic -> Evaluation.defaultEvaluator<'a> cspec loss
    match evalSpec with
    | Default -> baseEvaluator 
    | Cached cs -> Evaluation.cachedEvaluator<'a> baseEvaluator cs
    | Dropout (d,c) -> Evaluation.dropoutEvaluator<'a> baseEvaluator d c
        
        
  ///run a single generation
  let runGen cspec parent popSz (evaluator:Evaluator<_>) (test_cases:TestCases<_>) =
    let pop = genPop cspec 2 //exploratory genomes
    pop |> Array.iter (fun i -> i.Loss<-evaluator test_cases i.Genome)
    let bestPop = pop |> Array.minBy (fun i->i.Loss)
    let children = 
      [for i in 1 .. popSz do 
        let child = copyIndv parent
        mutate cspec child.Genome
        child.Loss <- evaluator test_cases child.Genome
        yield child
      ]
    let bestChild = children |> List.minBy (fun i -> i.Loss)
    //let child = copyIndv parent
    //mutate cspec rng child.Genome
    //child.Loss <- evaluator child.Genome
    let parent' = if parent.Loss < bestChild.Loss then parent else bestChild
    let parent' = 
      if parent'.Loss < bestPop.Loss then 
        parent' 
      else 
        bestPop
    parent'

  let createCache n =
    let name =  (sprintf "FsCgp_%d_%d" (DateTime.Now.ToFileTime()) n)
    new MemoryCache(name)

  ///one parent + lambda random individuals method
  ///each generation, the best individual is picked as parent
  ///only parent is mutated the rest are randomly generated
  ///currentBest is updated as new best indvidual is found
  //this could be a long running process so intermediate bests are useful for 
  //visualization and saving
  let inline run1PlusLambda 
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
    let parent = startIndv |> Option.defaultValue {Genome=randomGenome cspec; Loss=System.Double.MaxValue}

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
      else
        let parent' = runGen cspec parent lambda evaluator test_cases
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          postNewBest parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    loop 1 [] parent

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
            current := test_cases
            change := true
            match verbosity with Verbose -> printfn "test cases changed"  | _ -> ()
            )

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
      else
        let parent =
            if !change then
                let p = copyIndv parent
                p.Loss <- evaluator !current parent.Genome //update parent loss if test cases changed
                change := false
                p
            else
                parent
        let parent' = runGen cspec parent lambda evaluator !current
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          postNewBest parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    let initIndv = startIndv |> Option.defaultValue {Genome=randomGenome cspec; Loss=System.Double.MaxValue}
    loop 1 [] initIndv
