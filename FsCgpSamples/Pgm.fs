//#load "SetEnv.fsx"
//learn a function to fit polimonial data
open FsCgp
open FsCgp.CgpBase
open FsCgp.CgpRun
//open FsCgp.CgpGraph


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
//let evaluator = createEvaluator cspec loss Basic (Cached cacheSpec)
//let evaluator = createEvaluator cspec loss Parallel (Cached cacheSpec)
let evaluator = createEvaluator cspec loss Parallel (Dropout (0.1, 10))

let termination gen loss =
    List.head loss < 0.000001 //|| gen > 100000

let currentBest = ref Unchecked.defaultof<_>

let runAsync() =
  async {
    do runMuPlusLambda Verbose cspec 10 5 evaluator test_cases termination (fun indv -> currentBest.Value <- indv) None
  }
  //|> Async.Start

let showBest() = callGraph cspec currentBest.Value.Genome ///|> visualize
  
runAsync()  |> Async.RunSynchronously //run this to find the best genome
(*


showBest()  //run this periodically to view the graph of the current best genome

printGenome cspec currentBest.Value.Genome

*)

