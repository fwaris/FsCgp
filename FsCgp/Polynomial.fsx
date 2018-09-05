#load "XorshiftRng.fs"
#load "Cgp.fs"
#load "CgpRun.fs"
open FsCgp
open FsCgp.CgpBase
open FsCgp.CgpRun

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
      v * sign
    Evolve = fun i -> 
      let sign = if rng.NextDouble() > 0.5 then 1.0 else -1.0
      let v = rng.NextDouble()
      i + (sign * v)
  }

let spec = 
  {
    NumInputs = 1
    NumNodes = 30
    NumOutputs = 1
    BackLevel = 1+30
    FunctionTable = ft
    MutationRate = 0.20
    Constants = Some cnst
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

let termination gen loss = List.head loss = 0.0 || gen > 10000000

let indv = run1PlusLambda spec 4 rng evaluator termination
  
printGenome cspec indv.Genome

