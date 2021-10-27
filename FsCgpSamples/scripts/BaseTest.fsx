#I ".."
#load "../FsCgp/FsCgpProb.fs"
#load "../FsCgp/Cgp.fs"
open FsCgp
open FsCgp.CgpBase

//0-1-2-3-[0-1-2]-[3-4-5]-[6-7-8]-[9-10-11]-[12-13-14]-[15-16-17]
//         4       5       6       7         8          9
//[18-19-20]-[21-22-23]-[24-25-26]-[27-28-29]-30-31-32-33
// 10         11         12         13        o0 o1 o2 o3

let funcs =
  [|
    fun (xs:int[]) -> xs.[0] &&& xs.[1]       //and
    fun (xs:int[]) -> (~~~xs.[0]) &&& xs.[1]  //and with 1 input inverted
    fun (xs:int[])  -> xs.[0] ^^^ xs.[1]      //xor
    fun (xs:int[])  -> xs.[0] ||| xs.[1]      //or
  |]

let ft = funcs |> Array.map (fun f -> {F=f;Arity=2;Desc=""})

let s = 
  {
    NumInputs = 4
    NumOutputs = 4
    NumNodes  = 10
    BackLevel = None
    FunctionTable = ft
    MutationRate = 0.10
    Constants =  intConsts 1 1000 |> Some
  }

let cspec = compile s

let genome = randomGenome cspec 

let input = [|0;1;0;1|]

for i in 0 .. 100000 do 
  mutate cspec genome
  let out1 = evalGenome cspec genome input
  let ev = evaluator cspec genome
  let out2 = ev input
  if out1 = out2 |> not then failwithf "%A <> %A" out1 out2
  ()

//let g = createGenome cspec

evalGenome cspec genome input


let customGenome =
  {
    G =
      [|
        1;0;2 //funtion table 0 is constant function (if constants are used)
        1;0;3
        3;4;5
        1;1;2
        1;1;3
        2;5;7
        2;6;9
        1;5;7
        2;11;8
        1;11;8
        4;9;12;13
      |]
    Constants = [|0|]
  }

evalGenome cspec customGenome input