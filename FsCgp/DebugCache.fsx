#r "System.Runtime.Caching"
#load "XorshiftRng.fs"
#load "Cgp.fs"
#load "CgpRun.fs"
#r @"..\packages\Microsoft.Msagl.1.1.1\lib\net40\Microsoft.Msagl.dll"
#r @"..\packages\Microsoft.Msagl.Drawing.1.1.1\lib\net40\Microsoft.Msagl.Drawing.dll"
#r @"..\packages\Microsoft.Msagl.GraphViewerGDI.1.1.1\lib\net40\Microsoft.Msagl.GraphViewerGdi.dll"
#load "GgpGraph.fs"

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

let cnst = 
  {
    NumConstants = 1
    ConstGen = fun() -> 
      let sign = if rng.NextDouble() > 0.5 then 1.0 else -1.0
      let v = rng.NextDouble() * 100.0
      v * sign //|> int |> float
    Evolve = fun i -> 
      let sign = if rng.NextDouble() > 0.5 then 1.0 else -1.0
      let v = rng.NextDouble()
      i + (sign * v) //|> int |> float
  }

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

let termination gen loss = List.head loss < 0.001 || gen > 10000000

let currentBest = ref Unchecked.defaultof<_>

let runAsync() =
  async {
    do run1PlusLambda Verbose cspec 4 rng evaluator termination (fun indv -> currentBest := indv) None
  }
  |> Async.Start

let showBest() = callGraph cspec currentBest.Value.Genome |> visualize
  
(*
runAsync()  //run this to find the best genome

showBest()  //run this periodically to view the graph of the current best genome


*)

let gC = 
  {
    G =
       [|
         5;0;0;0;0;1;1;1;2;5;0;3;5;1;1;1;4;0;1;1;1;1;6;5;3;0;5;0;0;0;1;3;8;3;0;8;0;0;0;0;0;0;4;14;0;6;4;13;4;6
       //1     2     3     4
         8;1;0;5;3;6;5;3;18;17;0;0;3;5;9;18;6;11;1;2;11;10;0;0;0;4;22;1;3;13;25;2;12;1;5;8;11;6;0;2;4
       |]
    Constants =
       [|
         92.92507805
       |]
  }

let gI = 
  {
    G =
       [|
         5;0;0;0;0;1;1;1;2;5;1;3;5;1;1;1;4;0;4;1;1;1;6;2;3;0;5;0;0;0;1;3;9;3;0;8;0;0;0;0;0;0;4;14;0;6;4;13;4;6
      // 1     2     3     4
         8;1;0;5;3;6;4;3;18;17;0;0;3;5;9;19;6;11;1;2;11;10;0;0;0;4;22;1;3;13;25;2;12;1;5;8;11;6;0;2;4
       |]
    Constants =
       [|
         92.92507805
       |]
  }

(*
callGraph cspec gC |> visualize
callGraph cspec gI |> visualize
printGenome cspec gC
printGenome cspec gI
*)
let kC = genKey cspec gC
let kI = genKey cspec gI
kC = kI

let mC = genomeMask cspec gC
let mI = genomeMask cspec gI
mC = mI