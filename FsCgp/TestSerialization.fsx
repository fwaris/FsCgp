﻿#r "System.Runtime.Caching"
#load "XorshiftRng.fs"
#load "Cgp.fs"
#load "CgpRun.fs"
#r @"..\packages\Microsoft.Msagl.1.1.1\lib\net40\Microsoft.Msagl.dll"
#r @"..\packages\Microsoft.Msagl.Drawing.1.1.1\lib\net40\Microsoft.Msagl.Drawing.dll"
#r @"..\packages\Microsoft.Msagl.GraphViewerGDI.1.1.1\lib\net40\Microsoft.Msagl.GraphViewerGdi.dll"
#load "GgpGraph.fs"
#r @"..\packages\FsPickler.5.2\lib\net45\FsPickler.dll"
#load "CgpSer.fs"

open FsCgp
open FsCgp.CgpBase
open FsCgp.CgpGraph
open FsCgp

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
    BackLevel = None
    FunctionTable = ft
    MutationRate = 0.20
    Constants = Some cnst
    CacheWith = None
  }

let genome =
      {G = [|4; 0; 0; 4; 0; 1; 5; 2; 1; 4; 2; 2; 0; 0; 4; 0; 0; 1; 2; 0; 6; 0; 0; 4;
        2; 7; 3; 2; 1; 8; 3; 5; 8; 2; 5; 6; 5; 12; 8; 2; 10; 13; 2; 5; 2; 1; 5;
        13; 5; 4; 15; 2; 16; 16; 2; 15; 18; 3; 9; 13; 3; 19; 20; 0; 0; 12; 3;
        9; 5; 3; 18; 9; 0; 0; 12; 4; 0; 10; 2; 21; 18; 2; 14; 8; 3; 27; 11; 0;
        0; 20; 29|];
     Constants = [|5.000000841|];};


let cspec = compile spec
     
//printGenome cspec genome

let cg = callGraph cspec genome

visualize cg

CgpSer.saveGenomeTo "genome1.xml" genome //goes to <user>\AppData\Local\Temp

let g2:Genome<float> = CgpSer.loadGenomeFrom "genome1.xml"

let cg2 = callGraph cspec g2
visualize cg2
