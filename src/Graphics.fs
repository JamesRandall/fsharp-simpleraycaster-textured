module App.Graphics

// VSWAP.WL1
open System
open Browser.Dom
open Browser.Types
open Fable.Core
open App.Model


exception MissingGraphicsPackException

[<Emit("new ImageData($0,$1,$2)")>]
let createImageData (data:Fable.Core.JS.Uint8ClampedArray) width height : ImageData = jsNative

let setPixel (imageData:Texture) (color:UInt32) x y =
  let offset = int y*int imageData.Width + x
  imageData.Data.[offset] <- color
  
let getPixel (imageData:Texture) x y =
  let offset = int y*int imageData.Width + x
  imageData.Data.[offset]
   
let loadTextures () = async {
  // bit nasty - need to clean up, but if we switch to using the raw wolf images won't be needed anyway
  let textureWidth = 64.
  let textureHeight = 64.
  let imageDataArray = FSharp.Collections.Array.create 106 None
  {0..105}
  |> Seq.iter(fun index ->
    let image = Image.Create(textureWidth, textureHeight, src = $"textures/t{index}.png")
    let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
    canvas.width <- 64.
    canvas.height <- 64.
    image.onload <- (fun _ ->      
      let context = canvas.getContext_2d()
      context.drawImage(Fable.Core.U3.Case1 image, 0., 0., 64., 64.)
      let buffer = context.getImageData(0., 0., 64., 64.).data.buffer
      imageDataArray.[index] <-
        { Data = JS.Constructors.Uint32Array.Create(buffer)
          ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
          Width = 64
          Height = 64 } |> Some
    )
  )
  while imageDataArray |> Array.contains None do
    do! Async.Sleep (TimeSpan.FromMilliseconds 1000.)
  Fable.Core.JS.debugger ()
  return imageDataArray |> Array.map(fun element -> element.Value)
}

let loadGraphics () = async {
  // TODO: In here we want to render each texture to a canvas then we can use the existing drawing technique
  // (but with U3.Case2) and rely on the underlying (hopefully optimised) canvas drawImage implementation rather than
  // looping through pixels.
  
  let! vswapResult = Utils.loadAsset "VSWAP.WL1"
  let result =
    match vswapResult with
    | Ok vswap ->
      let vswapView = Fable.Core.JS.Constructors.DataView.Create vswap
      let wallTexturesOffset = vswapView.getUint32(6, true)
      vswapView,wallTexturesOffset
    | Error _ -> raise MissingGraphicsPackException
  return result
}
