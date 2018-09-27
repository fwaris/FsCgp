namespace FsCgp
open FsCgp.CgpBase

//serde 

module CgpSer = 
  open MBrace.FsPickler
  open System.IO

  let saveGenome<'a> (writer:TextWriter) (genome:Genome<'a>) =
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    ser.Serialize(writer,genome)

  let saveGenomeTo (path:string) genome =
    use str = File.CreateText(path)
    saveGenome str genome

  let saveIndv<'a> (writer:TextWriter) (genome:Genome<'a>) =
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    ser.Serialize(writer,genome)

  let saveIndvTo (path:string) (genome:Genome<'a>) =
    use str = File.CreateText(path)
    saveIndv str genome

  /// Deserializes the indvidual. 
  /// Caller should ensure context is valid for the genome (function table, inputs, etc)
  let loadIndv<'a> (reader:TextReader) : Indv<'a> = 
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    ser.Deserialize(reader)

  /// Deserializes the genome.
  /// Caller should ensure context is valid for the genome (function table, inputs, etc)
  let loadGeome<'a> (reader:TextReader) : Genome<'a> = 
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    ser.Deserialize(reader)

  /// Deserializes the indvidual.
  /// Caller should ensure context is valid for the genome (function table, inputs, etc)
  let loadIndvFrom (path:string) =
    use reader = File.OpenText path
    loadIndv reader

  /// Deserializes the genome.
  /// Caller should ensure context is valid for the genome (function table, inputs, etc)
  let loadGenomeFrom (path:string) =
    use reader = File.OpenText path
    loadGeome reader