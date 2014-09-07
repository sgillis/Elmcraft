module Model where

import Math.Vector3 (..)

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
