module App.Model

[<Measure>] type hp
[<Measure>] type points
[<Measure>] type ms

[<RequireQualifiedAccess>]
type Cell =
  | WithColor of hexColor:int
  | WithTexture of textureIndex:int
  | Empty
  
type Vector2D =
  { vX: float
    vY: float
  }
  member x.CrossProduct = { vX = x.vY ; vY = -x.vX }
  
type ControlState =
  | None          = 0b000000
  | Forward       = 0b000001
  | TurningLeft   = 0b000010
  | TurningRight  = 0b000100
  | StrafingLeft  = 0b001000
  | StrafingRight = 0b010000
  | Backward      = 0b100000

type Player =
  { Score: int<points>
    Health: int<hp>
  }

type Camera =
  { Position: Vector2D
    Direction: Vector2D
    Plane: Vector2D
  }
  
type Texture =
  { Data: Fable.Core.JS.Uint32Array
    ClampedData: Fable.Core.JS.Uint8ClampedArray
    Width: int
    Height: int
  }
  
// will type these when we know more about them - need to decompress and inspect firstx
type WolfensteinMap =
  { Width: int
    Height: int
    Plane0: obj
    Plane1: obj
    Plane2: obj
  }

type Game =
  { Map: Cell list list
    Player: Player
    Camera: Camera
    ControlState: ControlState
  }
  