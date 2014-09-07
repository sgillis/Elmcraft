module Display (scene) where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)
import Http (..)

import Model
import Display.World (ground)
import Display.Cube (cube)

scene : (Int, Int) -> Model.Person -> Response Texture -> Model.Blocks -> Element
scene (w, h) person texture blocks =
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) <| entities (view (w,h) person) texture blocks
           , container w h topLeft <| flow down
                [ vectorToText person.position
                , vectorToText <| Model.direction person
                , vectorToText person.velocity
                ]
           ]

entities : Mat4 -> Response Texture -> Model.Blocks -> [Entity]
entities view response blocks =
    case response of
        Success texture -> map (cube texture view) blocks
        _               -> []

view : (Int, Int) -> Model.Person -> Mat4
view (w, h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (lookAtMatrix person)

lookAtMatrix : Model.Person -> Mat4
lookAtMatrix person =
    makeLookAt 
        person.position
        (person.position `add` Model.direction person)
        j

vectorToText : Vec3 -> Element
vectorToText v = asText ( getX v, getY v, getZ v )
