module Model where

import Math.Vector3 (..)
import Math.Matrix4 (..)

data Inputs = TimeDelta Float { x:Int, y:Int } Bool Bool
            | Mouse (Int, Int)

type Person =
    { position        : Vec3 
    , velocity        : Vec3
    , horizontalAngle : Float
    , verticalAngle   : Float
    }

defaultPerson =
    { position        = vec3 0 2 -8
    , velocity        = vec3 0 0 0
    , horizontalAngle = pi/2
    , verticalAngle   = 0
    }

direction : Person -> Vec3
direction person =
    let h = person.horizontalAngle
        v = person.verticalAngle
    in vec3 (cos h) (sin v) (sin h)

type Block =
    { translation : Mat4 }

type Blocks = [Block]

initialWorld : Blocks
initialWorld =
    concat <|
    map oneRow [-20..20]

oneRow : Float -> Blocks
oneRow z =
    map (\x -> { translation = makeTranslate3  x -1 z }) [-20..20]
