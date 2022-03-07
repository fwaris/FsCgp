#r "nuget: FsPickler, Version=5.3.2"
#r "nuget: AutomaticGraphLayout.GraphViewerGDI"
#r "nuget: FSharp.Collections.ParallelSeq"
//#r "nuget: System.Configuration.ConfigurationManager, Version=6.0.0"
//#r "nuget: System.Runtime.Caching, Version=6.0.0"
//System.Reflection.Assembly.Load("System.Configuration.ConfigurationManager")
#I ".."
#load "../FsCgp/FsCgpObservableExt.fs"
#load "../FsCgp/FsCgpProb.fs"
#load "../FsCgp/Cgp.fs"
#load "../FsCgp/CgpRun.fs"
#I @"..\..\FsCgpTools"

//can't do visualization in .netcore and fsi yet
//#r "System.Windows.Forms"
//#r "Sytem.Drawing"
//#load "GgpGraph.fs"

