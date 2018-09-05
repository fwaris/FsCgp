namespace FsCgp
open XorshiftRng
open CgpBase

type Indv<'a> = {Genome:Genome<'a>; mutable Loss : float} //fitter individual has lower loss
type Evaluator<'a> = Genome<'a>->float

///generation -> loss history -> bool (true to terminate run)
type Terminator = int->float list->bool 

module CgpRun =

  let genPop cspec rng sz = [|for i in 0..sz-1 -> {Genome=randomGenome cspec rng; Loss=System.Double.MaxValue} |]

  let copyIndv i = {i with Genome=copyGenome i.Genome; Loss=i.Loss}

  ///a basic evaluator 
  ///where the test_cases are a seq of tuples of (input vector, output vector)
  ///loss_func takes genome output and actual ouput and returns calculated loss
  let defaultEvaluator cspec loss_func test_cases genome =
    let ev = evaluator cspec genome
    (0.0, test_cases) 
    ||> Seq.fold(fun accLoss (x,y) -> 
      let y' = ev x
      let caseLoss = loss_func y y'
      accLoss + caseLoss
    )

  ///run a single generation
  let runGen cspec rng parent popSz (evaluator:Evaluator<_>) =
    let pop = genPop cspec rng popSz
    pop |> Array.iter (fun i -> i.Loss<-evaluator i.Genome)
    let bestPop = pop |> Array.minBy (fun i->i.Loss)
    let child = copyIndv parent
    mutate cspec rng child.Genome
    child.Loss <- evaluator child.Genome
    let parent' = if parent.Loss < child.Loss then parent else child
    let parent' = if parent'.Loss < bestPop.Loss then parent' else bestPop
    parent'

  ///one parent + lambda random individuals method
  ///each generation, the best individual is picked as parent
  ///only parent is mutated the rest are randomly generated
  ///current parent is returned at termination
  let run1PlusLambda spec lambda (rng:XorshiftPRNG) (evaluator:Evaluator<_>) (terminator:Terminator) =
    let cspec = compile spec
    let parent = {Genome=randomGenome cspec rng; Loss=System.Double.MaxValue}

    let rec loop i hist parent =
      if not(List.isEmpty hist) && terminator i hist then 
        parent
      else
        let parent' = runGen cspec rng parent lambda evaluator
        loop (i+1) ((parent'.Loss::hist) |> List.truncate 5) parent'

    loop 1 [] parent
