module App.Map

open App.Model
open Fable.Core.JS
open Fable.SimpleHttp
open Browser

exception MapLoadException

let mapFromTextures source =
  source
  |> List.map(fun row ->
    row |> List.map(fun textureIndex ->
      if textureIndex > 0 then
        Cell.WithTexture (textureIndex-1)
      else
        Cell.Empty
    )  
  )

let mapFromColorIndexes source =
  let colours =
    [ 0xDC2626 // red
      0x16A34A // green
      0x2563EB // blue
      0xCA8A04 // yellow
      0x4B5563 // gray      
    ]
    
  source
  |> List.map(fun row ->
    row |> List.map(fun colourIndex ->
      if colourIndex > 0 then
        Cell.WithColor colours.[colourIndex-1]
      else
        Cell.Empty
    )  
  )
  
let rlewDecode (mapHeadView:DataView) (inView:DataView) =
  let rlewTag = mapHeadView.getUint16(0, true);
  let size = inView.getUint16(0, true);
  let buffer = Constructors.ArrayBuffer.Create(int size);
  let outView = Constructors.DataView.Create(buffer)
  console.log $"rlewSIZE: {outView.byteLength}"
  Seq.initInfinite(fun _ -> ())
  |> Seq.scan(fun (inOffset,outOffset) _ ->
    let w = inView.getUint16(inOffset, true)
    let inOffset = inOffset + 2
    if w = rlewTag then
      let n = inView.getUint16(inOffset, true)
      let x = inView.getUint16(inOffset + 2, true)
      {0..(int n-1)}
      |> Seq.iter(fun i ->
        outView.setUint16(outOffset+i*2, x, true)
      )
      (inOffset+4,outOffset+2*int n)
    else
      inOffset,outOffset  
  ) (2,0)
  |> Seq.takeWhile(fun (inOffset,_) -> inOffset < inView.byteLength)
  |> Seq.last
  |> ignore
  outView
  
let carmackDecode (inView:DataView) =
  let size = inView.getUint16(0,true)
  let buffer = Constructors.ArrayBuffer.Create(int size)
  let outView = Constructors.DataView.Create(buffer)
  
  console.log $"carmackSIZE: {outView.byteLength}"
  Seq.initInfinite(fun _ -> ())
  |> Seq.scan(fun (inOffset,outOffset) _ ->
    // possibly a pointer
    let x = inView.getUint8(inOffset + 1)
    if x = 0xA7uy || x = 0xA8uy then
      let n = inView.getUint8(inOffset)
      if n = 0uy then
        // exception (not really a pointer)
        outView.setUint8(outOffset, inView.getUint8(inOffset + 2))
        outView.setUint8(outOffset + 1, x)
        (inOffset+3,outOffset+2)
      elif x = 0xA7uy then
        // near pointer
        let offset = 2 * (inView.getUint8(inOffset + 2) |> int)
        {0..(int n-1)}
        |> Seq.iter(fun i ->
          let indexedOutOffset = outOffset+i*2
          outView.setUint16(indexedOutOffset, outView.getUint16(indexedOutOffset - offset, true), true);
        )
        (inOffset+3,outOffset+2*int n)
      else
        // far pointer
        let offset = 2 * int (inView.getUint16(inOffset + 2 |> int, true))
        {0..(int n-1)}
        |> Seq.iter(fun i ->
          let indexedOutOffset = outOffset+i*2
          outView.setUint16(indexedOutOffset, outView.getUint16(offset + 2 * i, true), true);
        )
        (inOffset+4,outOffset+2*int n)
    else
      outView.setUint16(outOffset, inView.getUint16(inOffset, true), true)
      (inOffset+2,outOffset+2)
  ) (2,0)
  |> Seq.takeWhile(fun (inOffset,_) -> inOffset < inView.byteLength)
  |> Seq.last
  |> ignore
  outView
  
let loadLevel levelIndex = async {
  let! gameMapsResult = Utils.loadAsset "GAMEMAPS.WL1"
  let! mapHeadResult = Utils.loadAsset "MAPHEAD.WL1"
  let result =
    match gameMapsResult,mapHeadResult with
    | Ok gameMaps, Ok mapHead ->
      let mapHeadView = Constructors.DataView.Create mapHead
      let offset = mapHeadView.getUint32(2 + 4 * levelIndex, true)
      let mapHeader = Constructors.DataView.Create(gameMaps, int offset, 42.)
      let plane0View =
        Constructors.DataView.Create (
          gameMaps, mapHeader.getUint32(0, true) |> int, mapHeader.getUint16(12, true) |> float
        )
      let plane0 = plane0View |> carmackDecode |> rlewDecode mapHeadView
      let plane1View =
        Constructors.DataView.Create (
          gameMaps, mapHeader.getUint32(4, true) |> int, mapHeader.getUint16(14, true) |> float
        )
      let plane1 = plane1View |> carmackDecode |> rlewDecode mapHeadView
      let plane2 =
        {0..63}
        |> Seq.map(fun rowIndex ->
          {0..63} |> Seq.map (fun colIndex -> false)  
        )
        |> Seq.toList
      { Width = int (sqrt (float (plane0.byteLength/2)))
        Height = int (sqrt (float (plane0.byteLength/2)))
        Plane0 = plane0
        Plane1 = plane1
        Plane2 = plane2
      }
    | _ -> raise MapLoadException
  return result
}

