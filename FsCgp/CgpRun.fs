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
  open System.Windows.Forms

  ///utility function to create ConstSpec for floats
  let floatConsts (rng:XorshiftPRNG) numConst maxConst = 
    {
      NumConstants = numConst
      ConstGen = fun() -> 
        let sign = if rng.NextDouble() > 0.5 then 1.0 else -1.0
        let v = rng.NextDouble() * maxConst
        v * sign //|> int |> float
      Evolve = fun i -> 
        let sign = if rng.NextDouble() > 0.5 then 1.0 else -1.0
        let v = rng.NextDouble()
        i + (sign * v) //|> int |> float
    }

  ///utility function to create ConstSpec for ints
  let intConsts (rng:XorshiftPRNG) numConst maxConst = 
    {
      NumConstants = numConst
      ConstGen = fun() -> 
        let sign = if rng.NextDouble() > 0.5 then 1 else -1
        let v = rng.Next(maxConst)
        v * sign //|> int |> float
      Evolve = fun i -> 
        let sign = if rng.NextDouble() > 0.5 then 1 else -1
        i + sign
    }


  ///function to convert float to string to be used as part of caching key
  let floatCache (f:float) = String.Format("{0:0.##################}",f)

  ///function to convert int to string to be used as part of caching key
  let intCache (f:int) = string f

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

  ///generate key for caching
  ///two functionally equivalent genomes should generate the same key
  let genKey cspec genome =
    let mask = genomeMask cspec genome
    let activeNodes = Seq.zip mask genome.G |> Seq.map(fun (b,n) -> (if b then string n else "-" ) |> box)
    let consts = 
      match cspec.Spec.CacheWith with
      | None -> Seq.empty
      | Some c2s -> genome.Constants |> Seq.map (fun c-> (c2s c) :> obj)
    let parms = Seq.append activeNodes consts
    let k = String.Join("_",Seq.toArray parms)
    k

  let setIndvLoss evaluator indv = indv.Loss <- evaluator indv.Genome

  let setIndvLossCache<'a> (cspec:SpecCache<'a>) (evaluator:Genome<'a>->float) indv =
    let key = genKey cspec indv.Genome
    let ci = MemoryCache.Default.Item key
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
        evaluator indv.Genome
    indv.Loss <- loss
    if ci = null then
      MemoryCache.Default.Add(key,indv.Loss,DateTimeOffset.Now.AddHours(2.0)) |> ignore

  ///run a single generation
  let runGen cspec rng parent popSz evaluator =
    let pop = genPop cspec rng 2 //exploratory genomes
    pop |> Array.iter (fun i -> i.Loss<-evaluator i.Genome)
    let bestPop = pop |> Array.minBy (fun i->i.Loss)
    let children = 
      [for i in 1 .. popSz do 
        let child = copyIndv parent
        mutate cspec rng child.Genome
        if cspec.Spec.CacheWith.IsSome then
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
    lambda                   //number of offspring
    (rng:XorshiftPRNG) 
    evaluator                //function to evaluate a genome's loss or fitness
    (terminator:Terminator) 
    pubishIndv               //function to post new best indvidual (this function should return immediately otherwise the evolutionary process will be slow)
    startIndv
    =
    let parent = startIndv |> Option.defaultValue {Genome=randomGenome cspec rng; Loss=System.Double.MaxValue}

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        match verbosity with Verbose -> printfn "done in gen %d" i | _ -> ()
      else
        let parent' = runGen cspec rng parent lambda evaluator
        if parent'.Loss < parent.Loss then
          match verbosity with Verbose -> printfn "new best %.10f" parent'.Loss | _ -> ()
          pubishIndv parent'
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    loop 1 [] parent
