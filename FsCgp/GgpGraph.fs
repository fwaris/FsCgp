namespace FsCgp
open FsCgp.CgpBase

module CgpGraph =
  open Microsoft.Msagl.GraphViewerGdi

  let nodeId = function 
    | Fun c -> sprintf "n%d=%s" c.Id c.Func.Desc
    | Input i  -> sprintf "Input %d" i
    | Out (i,r) -> sprintf "Out %d" i
    | Const (v) -> v |> box |> string

  let styleNode<'a> (n:Microsoft.Msagl.Drawing.Node) =
      let v = n.UserData :?> Node<'a>
      match v with
      | Fun c -> () 
      | Input i  -> n.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse
      | Out (i,r) -> n.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse
                     n.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LavenderBlush
                     n.Attr.LineWidth <- 2.0
      | Const (v) -> n.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LightGray


  let makeGraph<'a> (callGraph:Node<'a> array) =
    let g = new Microsoft.Msagl.Drawing.Graph()

    //nodes
    let allNodes = callGraph |> Seq.collect (fun n ->
      match n with
      | Fun c -> seq {yield n; yield! c.Refs}
      | Input i  -> seq {yield n}
      | Out (i,r) -> seq{yield n; yield r} 
      | Const (v) -> seq{yield n}
      )

    let drawingNodes = allNodes |> Seq.map (fun cn ->
      let n = g.AddNode(nodeId cn)
      n.UserData<-cn
      n)

    drawingNodes |> Seq.iter styleNode<'a>

    //edges
    callGraph |> Array.iter (function 
        | Fun c as cn -> c.Refs |> Array.iter(fun fromN -> g.AddEdge(nodeId fromN, nodeId cn) |> ignore)
        | Input _ | Const _ -> ()
        | Out (i,r) as cn -> g.AddEdge(nodeId r, nodeId cn) |> ignore)

    g

  ///visualize a graph given vertices, edges and subgraphs
  let visualize graph =
    let gv = new Microsoft.Msagl.GraphViewerGdi.GViewer()
    let g =  makeGraph graph
    g.Edges |> Seq.iter (fun e->  if e.Label<> null then e.Label.FontSize <- 8.0)
    gv.Graph <- g
    let f = new System.Windows.Forms.Form()
    f.SuspendLayout()
    gv.Dock <- System.Windows.Forms.DockStyle.Fill
    f.Controls.Add(gv)
    gv.Invalidate()
    gv.Update()
    f.ResumeLayout()
    f.Show()
