module App.Game

open App.Model
open Browser.Types
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open App.Render
open Browser
open Fable.Import


let sampleLevel = [                            //
  [ 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 2 ; 2 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ] // 6
  [ 1 ; 0 ; 2 ; 2 ; 2 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 15 ; 15 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 13 ; 15 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 15 ; 15 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ]
]

let textureWidth = 64.
let textureHeight = 64.
(*let private textures =
  {0..105}
  |> Seq.map(fun index ->
    let image = Image.Create(textureWidth, textureHeight, src = $"textures/t{index}.png")
    let canvas = window.document.createElement("canvas") :?> HTMLCanvasElement
    canvas.width <- 64.
    canvas.height <- 64.
    image.onload <- (fun _ ->      
      let context = canvas.getContext_2d()
      context.drawImage(U3.Case1 image, 0., 0., 64., 64.)
    )
    canvas
  )
  |> Seq.toList*)

let private initialGameState =
  { Map = sampleLevel |> App.Map.mapFromTextures // |> Map.mapFromColorIndexes
    Player = { Score = 0<points> ; Health = 100<hp> }
    Camera = {
      Position = { vX = 12. ; vY = 6. }
      Direction = { vX = -1. ; vY = 0. }
      Plane = { vX = 0. ; vY = 1. } // 0.666 }
    }
    ControlState = ControlState.None
  }

let private updateFrame game frameTime =
  let (|IsActive|_|) controlState game = if game.ControlState &&& controlState > ControlState.None then Some () else None 
  let frameMultiplier = float frameTime / 1000. 
  let movementSpeed = 5.0 * frameMultiplier // squares per second
  let rotationSpeed = 3.0 * frameMultiplier // radians per second
  let posX = game.Camera.Position.vX
  let posY = game.Camera.Position.vY
  let dirX = game.Camera.Direction.vX
  let dirY = game.Camera.Direction.vY
  let move speed inputGame =
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + dirX * speed)] = Cell.Empty then posX + (dirX * speed) else posX 
          vY = if game.Map.[int (posY + dirY * speed)].[int posX] = Cell.Empty then posY + (dirY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let strafe speed inputGame =
    let strafeDirection = game.Camera.Direction.CrossProduct
    let strafeX = strafeDirection.vX
    let strafeY = strafeDirection.vY
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + strafeX * speed)] = Cell.Empty then posX + (strafeX * speed) else posX 
          vY = if game.Map.[int (posY + strafeY * speed)].[int posX] = Cell.Empty then posY + (strafeY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let rotate inputGame =
    let rotationMultiplier =
      match inputGame with
      | IsActive ControlState.TurningRight -> -1.
      | IsActive ControlState.TurningLeft -> 1.
      | _ -> 0.
    let planeX = game.Camera.Plane.vX
    let planeY = game.Camera.Plane.vY
    let newDirX = dirX * cos(rotationMultiplier * rotationSpeed) - dirY * sin(rotationMultiplier * rotationSpeed)
    let newDirY = dirX * sin(rotationMultiplier * rotationSpeed) + dirY * cos(rotationMultiplier * rotationSpeed)
    let newPlaneX = planeX * cos(rotationMultiplier * rotationSpeed) - planeY * sin(rotationMultiplier * rotationSpeed)
    let newPlaneY = planeX * sin(rotationMultiplier * rotationSpeed) + planeY * cos(rotationMultiplier * rotationSpeed)
    { inputGame with
        Camera =
          { inputGame.Camera with
              Direction = { vX = newDirX  ; vY = newDirY}
              Plane = { vX = newPlaneX ;  vY = newPlaneY }
          }
    }

  game
  |> (fun g -> match g with | IsActive ControlState.Forward -> move movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.Backward -> move (-movementSpeed/2.) g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingLeft -> strafe -movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingRight -> strafe movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.TurningLeft | IsActive ControlState.TurningRight -> rotate g | _ -> g)
   
  
  
let private renderScene (context:CanvasRenderingContext2D) (textures:Texture array) game =
  let startTime = performance.now ()
  
  context.save ()
  context?imageSmoothingEnabled <- false
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  context.font <- "30px Consolas, Menlo, monospace"
  
  //clearCanvas context
  
  let width = context.canvas.width
  let height = context.canvas.height
  let ceilingColor = 0x393939
  let floorColor = 0x737373
  fill context ceilingColor -1. -1. (width+1.) (height/2.)
  fill context floorColor -1. (height/2.) (width+1.) ((height/2.)+1.)
  
  let outputImageData = context.getImageData(0., 0., width, height) 
  let outputTexture = {
    Data = Constructors.Uint32Array.Create(outputImageData.data.buffer)
    ClampedData = Constructors.Uint8ClampedArray.Create(outputImageData.data.buffer)
    Width = int width
    Height = int height
  }

  let posX = game.Camera.Position.vX
  let posY = game.Camera.Position.vY
  {(0.)..(width-1.)}
  |> Seq.iter(fun screenX ->
    let cameraX = (2. * screenX / width) - 1.
    let rayDirection = {
      vX = game.Camera.Direction.vX + game.Camera.Plane.vX * cameraX
      vY = game.Camera.Direction.vY + game.Camera.Plane.vY * cameraX
    }
    let mapX = int posX
    let mapY = int posY
    let deltaDistX = if rayDirection.vX = 0. then System.Double.MaxValue else abs (1. / rayDirection.vX)
    let deltaDistY = if rayDirection.vY = 0. then System.Double.MaxValue else abs (1. / rayDirection.vY)
    let stepX, initialSideDistX =
      if rayDirection.vX < 0. then
        -1,(posX - float mapX) * deltaDistX
      else
        1,(float mapX + 1.0 - posX) * deltaDistX
    let stepY, initialSideDistY =
      if rayDirection.vY < 0. then
        -1,(posY - float mapY)*deltaDistY
      else
        1,(float mapY + 1.0 - posY)*deltaDistY
    let _, sideDistX, sideDistY, hitMapX, hitMapY, side =
      Seq.initInfinite (fun _ -> 0)
      |> Seq.scan(fun (isHit, castSideDistX, castSideDistY, castMapX, castMapY, castSide) _ ->
        if isHit then
          (isHit, castSideDistX, castSideDistY, castMapX, castMapY, castSide)
        else
          let newSideDistX, newSideDistY, newMapX, newMapY, newSide =
            if castSideDistX < castSideDistY then
              castSideDistX + deltaDistX, castSideDistY, castMapX + stepX, castMapY, 0
            else
              castSideDistX, castSideDistY + deltaDistY, castMapX, castMapY + stepY, 1
          let newIsHit = game.Map.[newMapY].[newMapX] <> Cell.Empty
          newIsHit, newSideDistX, newSideDistY, newMapX, newMapY, newSide
      ) (false, initialSideDistX, initialSideDistY, mapX, mapY, 0)
      |> Seq.skipWhile (fun (isHit, _, _, _, _, _) -> not isHit)
      |> Seq.head
    let perpendicularWallDistance = if side = 0 then (sideDistX - deltaDistX) else (sideDistY - deltaDistY)
    let lineHeight = height / perpendicularWallDistance
    let startY = max (-lineHeight/2. + height/2.) 0.
    let endY = min (lineHeight/2. + height/2.) (height-1.)
    
    // just handling color maps at the moment
    match game.Map.[hitMapY].[hitMapX] with
    | Cell.WithColor color ->
      verticalLine context (if side = 1 then adjustColor color -30 else color) screenX startY endY
    | Cell.WithTexture textureIndex ->
      let wallX =
        if side = 0 then
          posY + perpendicularWallDistance * rayDirection.vY
        else
          posX + perpendicularWallDistance * rayDirection.vX
      let clampedWallX = wallX - (floor wallX)
      let rawTextureX = int (clampedWallX * textureWidth)
      let textureX =
        if side = 0 && rayDirection.vX > 0. then textureWidth - float rawTextureX - 1.
        elif side = 1 && rayDirection.vY < 0. then textureWidth - float rawTextureX - 1.
        else float rawTextureX
      let texture = textures.[textureIndex]
      
      // this is by far the quickest approach but results in weird artifacting when you are close to the 
      (*context.drawImage(
        U3.Case2 texture,
        textureX,
        0.,
        1.,
        textureHeight,
        screenX,
        startY,
        1.,
        endY-startY)*)
      
      let lineHeight = height / perpendicularWallDistance
      let step = 1.0 * textureHeight / lineHeight
      let texPos = (startY - height/2. + lineHeight/2.) * step
      
      {0..int (endY-startY)}
      |> Seq.iter(fun drawY ->
        let textureY = int (texPos+step*float drawY) &&& (int textureHeight-1)
        let color = Graphics.getPixel texture (int textureX) textureY
        Graphics.setPixel outputTexture color (int screenX) (drawY+int startY)
        //let colorStart = 4*int texture.width*textureY + 4*int textureX
        ()
        //let color = textureContext.getImageData(textureX, float textureY, 1., 1.).data |> Seq.take 4 |> Seq.toArray
        //let adjustedColor = if side = 1 then (color >>> 1) &&& 8355711 else color
        (*
        let pixelContext = context.createImageData(1.,1.)
        let pixelData = pixelContext.data
        pixelData.[0] <- texture.data.[colorStart]
        pixelData.[1] <- texture.data.[colorStart+1]
        pixelData.[2] <- texture.data.[colorStart+2]
        pixelData.[3] <- texture.data.[colorStart+3]
        context.putImageData(pixelContext, screenX, float drawY+startY)
        *)
      )
      
    | _ -> ()
  )
  //outputImageData.data <- JS.Constructors.Uint8Array.Create(outputTexture.ClampedData.buffer)
  let imageData = Graphics.createImageData outputTexture.ClampedData outputTexture.Width outputTexture.Height
  context.putImageData(imageData, 0., 0.)
  let endTime = performance.now()
  fillText context $"Render length: %.0f{endTime-startTime}ms" 32. 32.
  
  context.restore ()

let init (canvas:HTMLCanvasElement) = async {
  let! maps = App.Map.loadLevel 0
  let! graphics = App.Graphics.loadGraphics ()
  let! textures = App.Graphics.loadTextures ()
  let context = canvas.getContext_2d()
  let gameLoop (game:Game) (frameTime:float<ms>) =
    renderScene context textures game
    let updatedGameState = updateFrame game frameTime
    updatedGameState
    
  let updateControlState game controlState =
    { game with ControlState = game.ControlState ^^^ controlState }

  return gameLoop,updateControlState,initialGameState
}
  