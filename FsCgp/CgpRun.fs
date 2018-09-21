namespace FsCgp
open XorshiftRng
open CgpBase
open System
open System.Runtime.Caching

type Indv<'a> = {Genome:Genome<'a>; mutable Loss : float} //fitter individual has lower loss
type Evaluator<'a> = Genome<'a>->float

type Verbosity = Silent | Verbose

///generation -> loss history -> bool (true to terminate run)
type Terminator = int->float list->bool 

module CgpRun =
  open System.Threading

  let dumpIndv indv =
    printfn "{Genome = "
    dumpGenomeI indv.Genome 10
    printfn  " Loss = %f" indv.Loss
    printfn "}"

  let genPop (cspec:SpecCache<'a>) rng sz = [|for i in 0..sz-1 -> {Genome=randomGenome cspec rng; Loss=System.Double.MaxValue} |]

  let copyIndv i = {i with Genome=copyGenome i.Genome; Loss=i.Loss}

  ///a basic evaluator 
  ///where the test_cases are a seq of tuples of (input vector, output vector)
  ///loss_func takes genome output and actual ouput and returns calculated loss
  let defaultEvaluator cspec loss_func test_cases genome =
    let ev = evaluator cspec genome
    let fit =
      (0.0, test_cases) 
      ||> Seq.fold(fun accLoss (x,y) -> 
        let y' = ev x
        let caseLoss = loss_func y y'
        accLoss + caseLoss
      )
    if System.Double.IsNaN fit then System.Double.MaxValue else fit

  //evalutator that evaluates test cases in parallel

  ///Wrapper type for parallel evaluation
  type PEvalulator<'a>(cspec:SpecCache<'a>, genome) =
    let ev =  evaluator cspec genome
    member x.Evaluator = ev
  
  //evaluate test cases in parallel
  let defaultEvaluatorPar<'a> cspec (loss_func:'a[]->'a[]->float) test_cases genome =
    let ev = new ThreadLocal<PEvalulator<'a>>(fun _ -> PEvalulator(cspec,genome))
    let fit =
      test_cases 
      |> Array.Parallel.map (fun (x,y)->
        let y' = ev.Value.Evaluator x
        let caseLoss = loss_func y y'
        caseLoss)
      |> Array.sum
    if System.Double.IsNaN fit then System.Double.MaxValue else fit

  let genKey cspec genome =
    let genes = activeGenes cspec genome
    let activeNodes = Seq.zip genes genome.G |> Seq.map(fun (b,n) -> (if b then n else 0) |> box)
    let parms = Seq.append activeNodes (genome.Constants |> Seq.map box)
    let k = String.Join("_",Seq.toArray parms)
    k

  let setIndvLoss evaluator indv = indv.Loss <- evaluator indv.Genome

  let setIndvLossCache<'a> (cspec:SpecCache<'a>) (evaluator:Genome<'a>->float) indv =
    let key = genKey cspec indv.Genome
    let ci = MemoryCache.Default.Item key
    let loss =
      if ci <> null then
        let indv = ci :?> Indv<'a>
        indv.Loss
      else
        evaluator indv.Genome
    indv.Loss <- loss
    if ci = null then
      MemoryCache.Default.Add(key,indv,DateTimeOffset.Now.AddHours(2.0)) |> ignore


  ///run a single generation
  let runGen cspec rng parent popSz evaluator =
    let pop = genPop cspec rng 1 //exploratory genomes
    pop |> Array.iter (fun i -> i.Loss<-evaluator i.Genome)
    let bestPop = pop |> Array.minBy (fun i->i.Loss)
    let children = 
      [for i in 1 .. popSz do 
        let child = copyIndv parent
        mutate cspec rng child.Genome
        if cspec.Spec.UseCache then
          setIndvLossCache cspec evaluator child
        else
          setIndvLoss evaluator child
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
    (rng:XorshiftPRNG) 
    evaluator
    (terminator:Terminator) 
    pubishIndv
    startIndv
    =
    let parent = startIndv |> Option.defaultValue {Genome=randomGenome cspec rng; Loss=System.Double.MaxValue}

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        ()
      else
        let parent' = runGen cspec rng parent lambda evaluator
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          pubishIndv parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    loop 1 [] parent
