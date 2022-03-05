#load "SetEnv.fsx"
// dynamic version of polynomial learner
open FsCgp
open FsCgp.CgpBase
open FsCgp.CgpRun
//open FsCgp.CgpGraph
open System.Threading


//example taken from
//https://github.com/DataWraith/cgp

let funcs =
  [|
    (fun (xs:float[]) -> xs.[0]),2, "x"
    (fun (xs:float[]) -> xs.[1]),2, "y"
    (fun (xs:float[]) -> xs.[0] + xs.[1]),2,"add"
    (fun (xs:float[]) -> xs.[0] - xs.[1]),2,"subtract"
    (fun (xs:float[]) -> xs.[0] * xs.[1]),2,"multiply"
    (fun (xs:float[]) -> if xs.[1] = 0.0 then 0.0 else xs.[0] / xs.[1]),2,"division"
  |]

let ft = funcs |> Array.map (fun (f,a,d) -> {F=f;Arity=a;Desc=d})

let spec = 
  {
    NumInputs = 1
    NumNodes = 30
    NumOutputs = 1
    BackLevel = None
    FunctionTable = ft
    MutationRate = 0.20
    Constants = floatConsts 1 100.0 |> Some
  }

//points fitting f(x) = x³ - 2x + 10.
let test_cases =
  [|
        (0., 10.)
        (0.5, 9.125)
        (1., 9.)
        (10., 990.)
        (-5., -105.)
        (17., 4889.)
        (3.14, 34.679144)
  |]
  |> Array.map (fun (inp,out)-> [|inp|],[|out|])

let loss (y':float[]) (y:float[]) = (y'.[0] - y.[0]) ** 2.0 //square loss y' is output from the genome evaluation and y is actual output 

let cspec = compile spec

//********** cannot use caching with dynamic ***** 
//let cacheSpec = {Cache=createCache 1; Cspec=cspec; ConstGen=floatCache }
//let evaluator = createEvaluator cspec loss Basic (Cached cacheSpec)
//let evaluator = createEvaluator cspec loss Parallel (Cached cacheSpec)
let evaluator = createEvaluator cspec loss Parallel Default

let termination gen loss = gen > 100000

let currentBest = ref Unchecked.defaultof<_>

let cts = new  CancellationTokenSource()
let obsTestCases,fps = Observable.createObservableAgent cts.Token None //observable to send new data to learner

let runAsync() =
  async {
    do run1PlusLambdaDynamic 
        Verbose cspec 10  evaluator obsTestCases 
        termination (fun indv -> currentBest := indv) None
  }
  |> Async.Start

let showBest() = callGraph cspec currentBest.Value.Genome //|> visualize

let postTests() = fps test_cases //sends (new or updated) data to let the learner dynamically adapt to changing data
  
(*

runAsync()  //run this to find the best genome

postTests() //run this to update the test cases

showBest()  //run this periodically to view the graph of the current best genome

printGenome cspec currentBest.Value.Genome

*)

