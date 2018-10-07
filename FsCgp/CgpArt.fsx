#load "SetEnv.fsx"

open FsCgp.CgpGraph
open FsCgp
open FsCgp.CgpBase


let pi_255 = (System.Math.PI) / 255.0
let tpi_255 = 2.0 * (System.Math.PI) / 255.0
let t3pi_255 = 3.0 * (System.Math.PI) / 255.0
let pi_8_255 = System.Math.PI / (8. * 255.)


let funcs =
  [|
    (fun (xs:float[]) -> xs.[0]), "get_x"
    (fun (xs:float[]) -> xs.[1]), "get_y"
    (fun (xs:float[]) -> sqrt (xs.[0] + xs.[1])), "sqrt_sum"
    (fun (xs:float[]) -> sqrt (abs (xs.[0] - xs.[1]))), "sqrt_abs_diff"

    (fun (xs:float[]) -> 255.*(abs(sin(tpi_255 * xs.[0]) +  cos(tpi_255 * xs.[1]))/2.0)), "255*(|(sin(2π/255 * x) +  cos(2π/255 * y)|/2.0)"
    (fun (xs:float[]) -> 255.*(abs(cos(tpi_255 * xs.[0]) +  sin(tpi_255 * xs.[1]))/2.0)), "255*(|(cos(2π/255 * x) +  sin(2π/255 * y)|/2.0)"
    (fun (xs:float[]) -> 255.*(abs(cos(t3pi_255 * xs.[0]) +  sin(tpi_255 * xs.[1]))/2.0)), "255*(|(cos(3π/255 * x) +  sin(2π/255 * y)|/2.0)"
    (fun (xs:float[]) -> exp (xs.[0] + xs.[1]) % 256.), "exp (x + y)(mod 256)"
    (fun (xs:float[]) -> abs(sinh(xs.[0] + xs.[1])) % 256.), "|sinh(x + y)| (mod 256)"

    (fun (xs:float[]) -> cosh(xs.[0] + xs.[1]) % 256.), "cosh (x + y)(mod 256)"
    (fun (xs:float[]) -> 255. * abs(tanh(xs.[0] + xs.[1]))), "255|tanh(x + y)|"
    (fun (xs:float[]) -> 255. * abs(sin(pi_255 * (xs.[0] + xs.[1])))), "255(|sin(π/255(x + y))|"
    (fun (xs:float[]) -> 255. * abs(cos(pi_255 * (xs.[0] + xs.[1])))), "255(|cos(π/255(x + y))|"
    (fun (xs:float[]) -> 255. * abs(tan(pi_8_255 * (xs.[0] + xs.[1])))),"255(|tan(π/8∗255(x + y))|"

    (fun (xs:float[]) -> sqrt ((xs.[0] * xs.[0] + xs.[1] * xs.[1]) / 2.0)),"sqrt((x^2 + y^2) / 2)"
    (fun (xs:float[]) -> ((abs xs.[0]) ** xs.[1]) % 256.0),"|x|^y (mod 256)"
    (fun (xs:float[]) -> abs(xs.[0] + xs.[1]) % 256.), "|x + y| (mod 256)"
    (fun (xs:float[]) -> abs(xs.[0] - xs.[1]) % 256.), "|x − y| (mod 256)"
    (fun (xs:float[]) -> (xs.[0] * xs.[1]) / 255.), "xy/255"
    (fun (xs:float[]) -> if xs.[1] = 0. then xs.[0] else (xs.[0]/xs.[1]) ), "x/y"
  |]

let ft = funcs |> Array.map (fun (f,d) -> {F=f;Arity=2;Desc=d})


let s = 
  {
    NumInputs = 2
    NumNodes = 5
    NumOutputs = 3
    BackLevel = None
    FunctionTable = ft
    MutationRate = 0.20
    Constants = floatConsts 1 100. |> Some
  }

let cs = compile s

let cgn:Genome<float> =
  {
    G=
      [|
        5;1;0
        5;2;0
        9;3;3
        6;3;2
        13;2;5
        5;4;2
      |]
    Constants=[|0.0|]
  }

let getPixels (gn) = 
  let px = Array2D.create 256 256 [|0;0;0|]
  for x in 0. .. 255. do 
    for y in 0. .. 255. do 
      let inp = [|x;y|]
      let o = evalGenome cs gn inp |> Array.map int
      px.[int x, int y] <- o
  px

let getPixels2 (evaluator:float[]->float[])  = 
  let px = Array2D.create 256 256 [|0;0;0|]
  for x in 0. .. 255. do 
    for y in 0. .. 255. do 
      let inp = [|x;y|]
      let o = evaluator inp |> Array.map int
      px.[int x, int y] <- o
  px

#nowarn "9"

open System.Threading.Tasks
open System.Drawing
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

let genImage (px:int[][,]) =
    let imageHeight = 256
    let imageWidth = 256
    let pH1 = imageHeight - 1
    let pW1 = imageWidth - 1
    let bmp = new Bitmap(imageWidth,imageHeight)
    let data = bmp.LockBits(
                new Rectangle(0, 0, bmp.Width, bmp.Height),
                ImageLockMode.ReadWrite,
                bmp.PixelFormat)
    let ptr = data.Scan0
    let ptr:nativeptr<byte> = NativePtr.ofNativeInt ptr
    let channelStride = data.Stride //4 // argb

    Parallel.For(0, imageHeight, fun h ->
        Parallel.For(0, imageWidth,  fun w ->
            let rgb = px.[h,w]
            let i = (pH1 - h) * channelStride + (w * 4)
            NativePtr.set ptr i (byte rgb.[2]) //assume little endian arch. BGRA 
            NativePtr.set ptr (i+1) (byte rgb.[1])
            NativePtr.set ptr (i+2) (byte rgb.[0])
            NativePtr.set ptr (i+3) 255uy
            ) |> ignore
            ) |> ignore
    bmp.UnlockBits(data)
    bmp

open System.Windows.Forms

//let ev = evaluator cs cgn
//let px = getPixels2 ev

//evalGenome cs cgn [|1.;2.|]
//ev [|1.;2.|]

let showImage() =
  let fn = new Form()
  let img = new PictureBox()
  img.BackColor <- Color.BlueViolet
  img.Dock <- DockStyle.Fill
  img.SizeMode <- PictureBoxSizeMode.StretchImage
  fn.Controls.Add(img)
  let evaluator = evaluator cs cgn                       //optimized evalulator
  //let px = getPixels cgn
  let px2 = getPixels2 (evaluator:float[]->float[])
  let bmp = genImage px2
  img.Image <- bmp
  fn.Show()

showImage()



(*

// **** mutate genome to create new art ******

for _ in 0 .. 10 do
  for i in 0 .. 20 do mutate cs cgn
  showImage()

//visualize program graph
callGraph cs cgn |> visualize
*)

