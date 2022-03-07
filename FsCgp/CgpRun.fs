namespace FsCgp
open CgpBase
open System
open FSharp.Collections.ParallelSeq

type Indv<'a> = {Genome:Genome<'a>; mutable Loss : float} //fitter individual has lower loss
type TestCases<'a> = ('a[]*'a[])[]
type Evaluator<'a> = TestCases<'a>->Genome<'a>->float
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

  module private Evaluation =
      
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

  ///create threadsafe version of evaluator for parallelization
  let createEvaluator<'a> cspec loss : (('a[]*'a[])[] -> Genome<'a>->float) = 
    Evaluation.defaultEvaluatorPar<'a> cspec loss 

  let runParent cspec lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) parent =
    let children = 
        [1 .. lambda]
        |> PSeq.map(fun _ -> 
            let child = copyIndv parent
            mutate cspec child.Genome
            child.Loss <- evaluator test_cases child.Genome
            child)
        |> PSeq.toList
    let bestChild = children |> List.minBy (fun x->x.Loss)
    let parent' = if bestChild.Loss <= parent.Loss then bestChild else parent
    parent'
        
  ///'speciated' mu plus lambda - seems to perform better
  let runGenMuSpeciated cspec parents mu lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) =
    let explrtryPop = genPop cspec 2 //exploratory genomes
    explrtryPop |> PSeq.iter (fun i -> i.Loss<-evaluator test_cases i.Genome)
    let bestExpIndv = explrtryPop |> List.minBy (fun i->i.Loss)
    let children = parents |> PSeq.map (runParent cspec lambda evaluator test_cases) |> PSeq.toList
    let orderedIndvs = children @ [bestExpIndv] |> List.sortBy (fun i -> i.Loss)
    orderedIndvs |> List.truncate mu

  ///traditional mu + lambda 
  let runGenMu cspec parents mu lambda (evaluator:Evaluator<_>) (test_cases:TestCases<_>) =
    let explrtryPop = genPop cspec 2 //exploratory genomes
    let children = (parents |> PSeq.collect (fun p -> [for i in 1 .. lambda -> copyIndv p])) |> PSeq.toList
    children |> PSeq.iter (fun p -> mutate cspec p.Genome) 
    let children = children @ explrtryPop
    children |> PSeq.iter (fun c -> c.Loss <- evaluator test_cases c.Genome)
    let orderedIndvs = 
        Seq.append 
            (parents |> Seq.map (fun p -> p,1))               //parents with priority
            (children |> Seq.map (fun c -> c,0))              //children with higher priority
        |> Seq.sortBy (fun (i,priority) -> i.Loss,priority)   //select children before parents if the loss is the same
        |> Seq.map fst
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
    (evalMode:EvaluatorSpec<_>) 
    ///if true, evolves each parent separately to maintain diversity
    (speciated:bool)
    ///callback to get new best individual when found
    postNewBest 
    startIndv =
    let parents = 
        startIndv 
        |> Option.map(fun i -> genPop cspec (mu-1) @ [i])
        |> Option.defaultValue (genPop cspec mu)

    let rec loop i hist sample (parents:Indv<'a> list) =
        if not(List.isEmpty hist) && terminator i hist then 
            match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
        else
            let parents' = 
                if speciated then 
                    runGenMuSpeciated cspec parents mu lambda evaluator test_cases
                else
                    runGenMu cspec parents mu lambda evaluator test_cases
            if parents'.[0].Loss < parents.[0].Loss then
                match verbosity with Verbose -> printfn "new best %.10f" parents'.[0].Loss | _ -> ()
                postNewBest parents'.[0]
            let hist' = ((parents' |> List.map (fun x->x.Loss)) @ hist) |> List.truncate 5
            let sample =
                match evalMode with
                | Dropout(d,c) -> if i%c = 0 then 
                                        if verbosity = Verbosity.Verbose then
                                            printfn $"resampled test cases at gen {i}"
                                        Probability.Seq.sample (1.0 - d) test_cases |> Seq.toArray 
                                  else 
                                    sample
                | Default      -> sample
            loop (i+1) hist' sample parents'
    loop 1 [] test_cases parents


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
        let parent' = runGenMu cspec [parent] 1 lambda evaluator current.Value |> List.head                
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          postNewBest parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    let initIndv = startIndv |> Option.defaultValue {Genome=randomGenome cspec; Loss=System.Double.MaxValue}
    loop 1 [] initIndv
