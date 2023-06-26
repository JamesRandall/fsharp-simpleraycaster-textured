module App
open App.Model
open Browser.Dom
open Fable.Core
let canvas = document.querySelector(".game-canvas") :?> Browser.Types.HTMLCanvasElement
let setCanvasSize _ =
  canvas.width <- window.innerWidth
  canvas.height <- window.innerHeight
window.onresize <- setCanvasSize
setCanvasSize ()

let private controlStateFromKeyCode keyCode =
  match keyCode with
  | "KeyW" -> ControlState.Forward
  | "KeyA" -> ControlState.TurningLeft
  | "KeyD" -> ControlState.TurningRight
  | "KeyQ" -> ControlState.StrafingLeft
  | "KeyE" -> ControlState.StrafingRight
  | "KeyS" -> ControlState.Backward
  | _ -> ControlState.None

let initialisationPromise = (App.Game.init canvas |> Async.StartAsPromise)
initialisationPromise
  .``then``(fun (gameLoop,controlStateHandler,initialGameState) ->
    let mutable previousTimestamp = 0.
    let mutable currentGameState = initialGameState
    let rec wrappedGameLoop timestamp =
      let timeInFrame = timestamp - previousTimestamp
      previousTimestamp <- timestamp
      currentGameState <- gameLoop currentGameState (timeInFrame * 1.<ms>)
      window.requestAnimationFrame wrappedGameLoop |> ignore
    window.onkeydown <- (fun ke ->
      if not ke.repeat then
        let controlState = ke.code |> controlStateFromKeyCode
        if controlState <> ControlState.None then
          ke.preventDefault()
          currentGameState <- controlStateHandler currentGameState controlState
    )
    window.onkeyup <- (fun ke ->
      let controlState = ke.code |> controlStateFromKeyCode
      if controlState <> ControlState.None then
        ke.preventDefault()
        currentGameState <- controlStateHandler currentGameState controlState
    )
    window.requestAnimationFrame wrappedGameLoop |> ignore
  )
  .catch(fun exn -> console.error $"Error initialising engine {exn}")
  |> ignore

