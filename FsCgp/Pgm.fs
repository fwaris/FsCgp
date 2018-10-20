//for debugging only
module CgpPgm

System.Console.WriteLine ("debugging console.")

//********** test code below *******
open FsCgp
open System
open CgpBase
let ATTACK_INPUTS = 11
let CHAR_INPUTS = 8
let NUM_INPUTS = (ATTACK_INPUTS + CHAR_INPUTS) * 2
let MAX_CASES = 10000

let funcs =
    [|
    (fun (xs:float[]) -> xs.[0]),4, "x1"
    (fun (xs:float[]) -> xs.[1]),4, "x2"
    (fun (xs:float[]) -> xs.[2]),4, "x3"
    (fun (xs:float[]) -> xs.[3]),4, "x4"
    (fun (xs:float[]) -> xs.[0] + xs.[1]),2,"add"
    (fun (xs:float[]) -> xs.[0] - xs.[1]),2,"subtract"
    (fun (xs:float[]) -> xs.[0] * xs.[1]),2,"multiply"  
    (fun (xs:float[]) -> if xs.[1] = 0.0 then 0.0 else xs.[0] / xs.[1]),2,"division"
    (fun (xs:float[]) -> log xs.[0]),1,"log"
    (fun (xs:float[]) -> exp xs.[0]),1,"exp"
    (fun (xs:float[]) -> abs xs.[0]),1,"abs"
    (fun (xs:float[]) -> (1.0 - (1.0 + (Math.E ** (-xs.[0]))))),1,"sigmod"
    (fun (xs:float[]) -> tanh xs.[0]),1,"tanh"
    (fun (xs:float[]) -> max xs.[0] xs.[1]),2,"max"
    (fun (xs:float[]) -> min xs.[0] xs.[1]),2,"min"
    (fun (xs:float[]) -> max xs.[0] 0.0),1,"ReLU"
    (fun (xs:float[]) -> if xs.[0] > 0.0 then xs.[1] else xs.[2]),3,"if x > 0."
    (fun (xs:float[]) -> if xs.[0] > xs.[1] then xs.[2] else xs.[3]),4,"if x > y."
    |]

let ft = funcs |> Array.map (fun (f,a,d) -> {F=f;Arity=a;Desc=d})

let spec = 
  {
    NumInputs = NUM_INPUTS
    NumNodes = 10
    NumOutputs = 56  //actions
    BackLevel = None
    FunctionTable = ft
    MutationRate = 0.20
    Constants = floatConsts 5 100.0 |> Some
  }

let cspec = compile spec

let g = 
    {
      G = 
         [|
           3;1;29;1;4;1;8;0;21;10;0;0;0;0;39;17;15;1;25;0;18;0;0;0;18;1;0;0;21;0;6;0;0;0;0;0;7;20;1;3;0;0;0;0;0;0;27;11;0;0
           21;0;4;0;0;10;0;11;13;0;15;0;14;0;4;39;4;4;0;0;0;47;47;0;18;0;0;0;44;33;0;8;17;3;0;0;18;0;42;0;14;0;7;46;0;23;11;0;14;0
           0;0;0;0;13;16
         |]
      Constants = 
         [|
           -65.15144794
           48.48539538
           15.52265524
           79.66986274
           100.0
         |]
    }

let cg = callGraph cspec g



//********** test code above *******
System.Console.WriteLine ("press enter to terminate")
System.Console.ReadLine() |> ignore