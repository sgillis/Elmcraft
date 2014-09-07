module Update (step) where

import Math.Vector3 (..)
import Math.Vector3 as V3
import Math.Matrix4 (..)

import Model

step : Model.Inputs -> Model.Person -> Model.Person
step inputs person =
    case inputs of
        Model.Mouse movement   -> turn movement person
        Model.TimeDelta dt dir shift space
            -> person |> move dir
                      |> down shift
                      |> up space
                      |> physics dt

move : {x:Int, y:Int} -> Model.Person -> Model.Person
move wasd person = 
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir
        move = V3.scale (toFloat wasd.y) moveDir
        strafe = V3.scale (toFloat wasd.x) strafeDir
        direction = move `add` strafe
    in
        { person | velocity <- adjustVelocity direction }

down : Bool -> Model.Person -> Model.Person
down shift person =
    if shift then
        { person | velocity <- adjustVelocity <| vec3 0 -1 0 }
    else
        person

up : Bool -> Model.Person -> Model.Person
up space person =
    if space then
        { person | velocity <- adjustVelocity <| vec3 0 1 0 }
    else
        person

adjustVelocity : Vec3 -> Vec3
adjustVelocity v =
    case toTuple v of
      (0,0,0) -> v
      _       -> V3.scale 2 (normalize v)


turn : (Int, Int) -> Model.Person -> Model.Person
turn (h, v) person =
    let h' = person.horizontalAngle + toFloat h / 500
        v' = person.verticalAngle - toFloat v / 500
    in {person | horizontalAngle <- h'
               , verticalAngle   <- clamp (degrees -45) (degrees 45) v' }

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

physics : Float -> Model.Person -> Model.Person
physics dt person =
    let position' = person.position `add` V3.scale dt person.velocity
    in { person | position <- position' }
