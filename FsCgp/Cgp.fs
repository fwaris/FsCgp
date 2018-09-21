namespace FsCgp
//Please read this for background
//Chapter 2. Cartesian Genetic Programming. Julian F. Miller.  
//https://www.springer.com/cda/content/document/cda_downloaddocument/9783642173097-c2.pdf


///entry of the function table
type Func<'a> =
  {
    F     : 'a[]->'a
    Arity : int
    Desc  : string
  }
  
///treatement of constant values are specified by this structure
type ConstSpec<'a> = 
  {
    NumConstants : int
    ConstGen     : unit->'a
    Evolve       : 'a->'a
  }

type Genome<'a> = {G:int[]; Constants:'a[]} //Note:'G' and Constants arrays are mutated as part of the process

///specifications for CGP 
type Spec<'a> = 
  {
    NumInputs     : int              //size of input vector
    NumOutputs    : int              //size of output vector
    NumNodes      : int              //number function nodes
    BackLevel     : int option       //how many levels can go back for node input or None for default of all
    FunctionTable : Func<'a>[]       //function table
    MutationRate  : float                   //the %age of genes to mutate
    Constants     : ConstSpec<'a> option    //number of constants
    UseCache      : bool 
}

///cached specification created by the 'compile' function (not to be directly created by user)
type SpecCache<'a> =
  {
    Spec           : Spec<'a>
    NodeLen        : int
    NumMtn         : int
    OutputOffset   : int
    GenomeSize     : int
    RefSize        : int
    NodesEndIdx    : int
    MaxArity       : int
    FtbleWithConst : Func<'a>[]
    NumCnsts       : int
    FConstIdx      : int
    EffBkLvl       : int
  }

module CgpBase =
  open XorshiftRng
  open System.Text

  ///check settings and cache some values to avoid needless computation
  let compile spec = 

    let ft = 
      match spec.Constants with
      | None -> spec.FunctionTable
      | Some c -> 
        let cFuncs = [| {F=(fun xs->xs.[0]); Arity=1; Desc="const function"} |] //constant function is always first, idx=0
        Array.append cFuncs spec.FunctionTable

    let intNodes = [spec.NumNodes; spec.NumInputs; spec.NumOutputs]
    if List.exists (fun x->x<=0) intNodes then failwith "integer values in spec must be positive"
    if spec.MutationRate < 0.0 then failwith "mutation rate is negative"
    if spec.FunctionTable.Length = 0 then failwith "empty function table"
    let maxBackLevel =  spec.NumInputs + spec.NumNodes
    let backLevel = spec.BackLevel |> Option.defaultValue maxBackLevel
    let backLevel = min backLevel maxBackLevel //cap to max back
    let maxArity = ft |> Array.map(fun x->x.Arity) |> Array.max
    let nodeLen = 1 (*function gene*) + maxArity (*connection genes*)
    let genomeSize = (nodeLen * spec.NumNodes) + spec.NumOutputs
    let numConsts = match spec.Constants with None -> 0 | Some c-> c.NumConstants 
    {
      Spec          = spec
      NodeLen       = nodeLen
      NumMtn        = spec.MutationRate * float (spec.NumNodes + spec.NumOutputs) |> int |> max 1 //at least 1 mutation
      OutputOffset  = nodeLen * spec.NumNodes
      GenomeSize    = genomeSize
      RefSize       = spec.NumInputs + spec.NumNodes
      NodesEndIdx   = genomeSize-spec.NumOutputs-1
      MaxArity      = maxArity
      FtbleWithConst=ft
      NumCnsts      = numConsts
      FConstIdx     = 0
      EffBkLvl     = backLevel
    }
  
  //utility functions
  let isInput cspec id = id < cspec.Spec.NumInputs
  let isOutput cspec id = id >= cspec.OutputOffset
  let isConstant cspec funcId = cspec.NumCnsts > 0 && funcId = cspec.FConstIdx
  let isFunction cspec i = if i > cspec.NodesEndIdx then false else i % cspec.NodeLen = 0
  let genomeOffset cspec nodeId = (nodeId - cspec.Spec.NumInputs) * cspec.NodeLen 

  ///node id (function + connection) to which the given index i belongs
  let nodeIdx cspec i =  
    if i > cspec.NodesEndIdx then failwithf "Invalid node index"
    (i / cspec.NodeLen) + cspec.Spec.NumInputs


  ///returns a (not thread safe) optimized function, for the given genome, 
  ///that can be used to evaluate an input vector
  //eliminiates array allocations for individual function evaluations
  //allocates only the output array
  //useful when a large number (could be in the millions)
  //of evaluations per genome are needed (to measure fitness, e.g.) 
  let evaluator<'a> cspec (genome:Genome<'a>) =
  
    let functionOutputs = Array.zeroCreate cspec.RefSize  //places where the evaluation results of a function will go
    let isVisited = Array.zeroCreate cspec.RefSize        //have we visited this node
    let mutable functionCalls = []                        //order in which functions will be evaluated
  
    let inputs = ref (Array.zeroCreate 1)                 //this will be replaced by actual input so no need to allocate large array

    let rec processNode (out:'a[],i) nodeId = 
      //printfn "eval nodeId %d" nodeId

      if isVisited.[nodeId] then                        //A: if visited
        let outs = functionOutputs.[nodeId]          
        functionOutputs.[nodeId] <- ((out,i)::outs)     //append output location and index to existing list 

      elif isInput cspec nodeId then                    //B: if input (nodeId points to an input location)
        let valF = fun (outs:('a[]*int) list) -> 
          let inp = inputs.Value.[nodeId]
          for out,i in outs do
            out.[i] <- inp                              //stuff the particular input into output slots
        isVisited.[nodeId] <- true                      //node visited
        functionOutputs.[nodeId] <- [(out,i)]           //save location to where this input value will go
        functionCalls <- (nodeId,valF)::functionCalls   //push to establish evaluation order

      else                                              //C: nodeId is for a function gene
        let gOffst = genomeOffset cspec nodeId
        //printfn "gOffst %d" gOffst
        let f = cspec.FtbleWithConst.[genome.G.[gOffst]]                  //lookup function in function table
        isVisited.[nodeId] <- true                                            //node visited
        functionOutputs.[nodeId] <- [(out,i)]                                 //save location to where the output of this func. will go
        if isConstant cspec  genome.G.[gOffst] then
          let valF = fun (outs:('a[]*int) list) ->                            //fun to run the evaluation of lookup function
            let cRef = genome.G.[gOffst + 1]
            let fOut =  genome.Constants.[cRef]                               //evaluate function
            for out,i in outs do                                                
              out.[i] <- fOut                                                  //set outputs
          functionCalls <- (nodeId,valF)::functionCalls
        else
          let fInputs = Array.zeroCreate f.Arity                                //allocate array to hold function inputs
          let goIdx = [|for j in gOffst + 1 .. gOffst+f.Arity -> j |]           //index of the connection genes
          goIdx |> Array.iteri (fun i j -> processNode (fInputs,i) genome.G.[j])  //recursive call to determine values of inputs
          let valF = fun (outs:('a[]*int) list) ->                              //fun to run the evaluation of lookup function
            let fOut =  f.F fInputs                                             //evaluate function
            for out,i in outs do                                                
              out.[i] <- fOut                                                   //set outputs
          functionCalls <- (nodeId,valF)::functionCalls                         //push to establish evaluation order


    let output = Array.zeroCreate cspec.Spec.NumOutputs                       //allocate array to hold outputs
    let goIdx = [| for j in cspec.OutputOffset  .. genome.G.Length-1 -> j |]  //index of genes providing output
    goIdx |> Array.iteri (fun i j -> processNode (output,i) genome.G.[j])     //process each gene 

    let fcalls = List.rev functionCalls //reverse list (FIFO)

    //return the evaluator function
    fun realInputs ->
      inputs := realInputs
      fcalls |> List.iter (fun (foOffset,f) -> 
        let funcOutputs = functionOutputs.[foOffset]
        f funcOutputs
      )
      Array.copy output

  ///evaluates genome against single input vector
  ///thread safe but allocates intermediate arrays
  let evalGenome<'a> cspec (genome:Genome<'a>) (inputs:'a[]) =
    let vals = Array.zeroCreate cspec.Spec.NumNodes
    let hasVals = Array.zeroCreate cspec.Spec.NumNodes

    let rec evalNode nodeId = 
      //printfn "eval nodeId %d" nodeId
      if isInput cspec nodeId then 
        inputs.[nodeId]
      else
        let valOffset = nodeId-cspec.Spec.NumInputs
        if hasVals.[valOffset] then vals.[valOffset]
        else
          let gOffst = genomeOffset cspec nodeId
          //printfn "gOffst %d" gOffst
          let funcId = genome.G.[gOffst]
          if isConstant cspec funcId then
            let cRef = genome.G.[gOffst + 1]
            let v = genome.Constants.[cRef]
            vals.[valOffset] <- v
            hasVals.[valOffset] <- true
            v
          else
            let f = cspec.FtbleWithConst.[funcId]
            let inps = [|for inp in genome.G.[gOffst + 1 .. gOffst+f.Arity] -> evalNode inp|] 
            let v = f.F inps
            vals.[valOffset] <- v
            hasVals.[valOffset] <- true
            v
    [| for i in cspec.OutputOffset  .. genome.G.Length-1-> evalNode genome.G.[i] |]

  ///mutate the genome at the given index honoring allele constraints
  let mutateAt cspec (rng:XorshiftPRNG) (genome:Genome<_>) idx =
      if isOutput cspec idx then                                        //output node 
        let n = rng.Next(cspec.RefSize)                                 //point to random upstream node
        genome.G.[idx] <- n
      elif isFunction cspec idx then                                    //function node
        let n = rng.Next(cspec.FtbleWithConst.Length)               
        genome.G.[idx] <- n
        if isConstant cspec n then                                      //if 'constant' function was chosen
          let c = rng.Next(genome.Constants.Length)                     //set parameter to valid constant
          genome.G.[idx+1] <- c
      else 
        let nodeId = nodeIdx cspec idx                                        //get function gene for chosen connection
        let gOffst = genomeOffset cspec nodeId
        if isConstant cspec genome.G.[gOffst] then                              //if points to constant 
          let cRef = genome.G.[gOffst + 1]
          let c2 = cspec.Spec.Constants.Value.Evolve genome.Constants.[cRef]   //evovle constant
          genome.Constants.[cRef]<-c2
        else
          let minRange = max (nodeId - 1 - cspec.EffBkLvl) 0            //regular connection 
          let maxRange = nodeId
          let n = rng.Next(minRange,maxRange) 
          genome.G.[idx] <- n                                                 //set to random node

  //perform a point mutation at a random location
  let mutate1 cspec (rng:XorshiftPRNG) genome =
      let idx = rng.Next(cspec.GenomeSize)
      mutateAt cspec rng genome idx

  ///mutate genome using spec mutation rate
  let mutate cspec rng genome =
    for _ in 0..cspec.NumMtn-1 do
      mutate1 cspec rng genome

  ///create a genome from a compiled spec
  let createGenome cspec = 
    let cnsts = if cspec.NumCnsts = 0 then [||] else [|for i in 1..cspec.NumCnsts -> cspec.Spec.Constants.Value.ConstGen()|]
    {G=Array.zeroCreate cspec.GenomeSize; Constants=cnsts}

  ///return a new randomized genome that honors the allele constraints
  let randomGenome<'a> (cspec:SpecCache<'a>) rng = 
    let g = createGenome cspec
    for i in 0..cspec.GenomeSize-1 do
      mutateAt cspec rng  g i
    g

  let copyGenome g = {g with G=Array.copy g.G; Constants=Array.copy g.Constants}

  let activeGenes cspec genome =
    let genes = Array.zeroCreate cspec.Spec.NumNodes

    let rec markNode nodeId = 
      //printfn "eval nodeId %d" nodeId
      if isInput cspec nodeId then 
        ()
      else
        let valOffset = nodeId-cspec.Spec.NumInputs
        genes.[valOffset] <- true
        let gOffst = genomeOffset cspec nodeId
        //printfn "gOffst %d" gOffst
        let funcId = genome.G.[gOffst]
        if isConstant cspec funcId then
          ()
        else
          let f = cspec.FtbleWithConst.[funcId]
          for inp in genome.G.[gOffst + 1 .. gOffst+f.Arity] do 
            markNode inp
    for i in cspec.OutputOffset  .. genome.G.Length-1 do 
      markNode genome.G.[i]
    genes

  type Call<'a> = {Id:int; Func:Func<'a>; mutable Refs:Node<'a> []}
  and Node<'a> = Const of 'a | Fun of Call<'a> | Input of int | Out of int * Node<'a>

  let callGraph<'a> (cspec:SpecCache<'a>) (genome:Genome<'a>) =
    let genes = activeGenes cspec genome
    let nodes = genes |> Array.mapi (fun i g -> i,g) |> Array.filter snd
    let nodes = 
      nodes 
      |> Array.map (fun (idx,_) -> 
        let nodeId = idx + cspec.Spec.NumInputs
        let gOffst = genomeOffset cspec nodeId
        let funcId = genome.G.[gOffst]
        if isConstant cspec funcId then
          let c = genome.Constants.[genome.G.[gOffst + 1]]
          nodeId,Const(c)
        else
          let ft = cspec.FtbleWithConst.[funcId]
          nodeId,Fun {Id=nodeId; Func=ft; Refs=[||]})
      |> Map.ofArray
    let calls =
      nodes 
      |> Map.toArray 
      |> Array.choose (fun (nodeId,node) ->
        match node with
        | Const _ | Input _ | Out _-> None 
        | Fun c -> 
            let gOffst = genomeOffset cspec nodeId
            let refs = genome.G.[gOffst+1..gOffst+c.Func.Arity]
            let refNodes = 
              refs
              |> Array.map (fun i ->
                if isInput cspec i  then
                  Input i
                else
                  nodes.[i])
            c.Refs <- refNodes
            Some (Fun c))
    let outs =
      [|
        for i in cspec.OutputOffset .. cspec.GenomeSize-1 do
          let refIdx = genome.G.[i]
          let outIdx = i - cspec.OutputOffset
          if isInput cspec refIdx then
            yield Out(outIdx,Input refIdx)
          else
            yield Out(outIdx,nodes.[refIdx])
      |]
    Array.append calls outs
   
  let printGenome<'a> (cspec:SpecCache<'a>) (genome:Genome<'a>) =
    let cg = callGraph cspec genome
    let sb = StringBuilder()
    let (!>) s = sb.Append(s:string) |> ignore
    let ln() = sb.AppendLine() |> ignore
    let printNode = function 
        | Input i -> !>"input["; !> (string i); !>"] "
        | Fun c   -> !> "n"; !> (string c.Id); !> " "
        | Const c -> !> (c |> box |> string); !> " "
        | Out _   -> ()
    cg |> Array.iter (fun n ->
      match n with
      | Fun c ->
        !> "let n"; !> (string c.Id); !> " = "; !> c.Func.Desc; !> " "
        c.Refs |> Array.iter printNode
      | Out (i,n) ->
        !> "o"; !> (string i); !> " = "; printNode n; !> ", "
      | _ -> ()
      ln())
    sb.ToString()

  let dumpGenomeI genome indent =
    let ids = new System.String([|for _ in 1 .. indent -> ' '|])
    let (!) s = printf "%s" ids; printfn "%s" s
    ! "{"
    ! "  G = "
    ! "     [|"
    genome.G 
                |> Seq.chunkBySize 50 
                |> Seq.iter (fun c -> ! (sprintf "       %s"  (System.String.Join(";",c))))
    ! "     |]"
    ! "  Constants = "
    ! "     [|"
    genome.Constants |> Seq.iter (fun v -> ! (sprintf "       %A" v))
    ! "     |]"
    ! "}"
    
  let dumpGenome genome = dumpGenomeI genome 0