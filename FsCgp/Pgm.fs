//for debugging only
module CgpPgm

System.Console.WriteLine ("debugging console.")

//********** test code below *******
open FsCgp
open FsCgp.CgpBase
open FsCgp.CgpRun
open FsCgp.CgpGraph

let rng = new XorshiftRng.XorshiftPRNG()

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
    Constants = floatConsts rng 1 100.0 |> Some
    CacheWith = Some floatCache
  }

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

let evaluator = defaultEvaluator cspec loss test_cases
//let evaluator = defaultEvaluatorPar cspec loss test_cases

let termination gen loss = List.head loss < 0.001 //|| gen > 100000

let currentBest = ref Unchecked.defaultof<_>

let runAsync() =
  async {
    do run1PlusLambda Verbose cspec 4 rng evaluator termination (fun indv -> currentBest := indv) None
  }
  |> Async.Start

runAsync()

//System.Console.WriteLine s

//********** test code above *******
System.Console.WriteLine ("press enter to terminate")
System.Console.ReadLine() |> ignore