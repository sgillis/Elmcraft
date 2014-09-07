import Window
import Keyboard
import Mouse

import Graphics.WebGL (loadTexture)

import Model
import Display
import Update

-- Pointer Lock
port movement : Signal (Int,Int)
port isLocked : Signal Bool

port requestPointerLock : Signal ()
port requestPointerLock =
    dropWhen (lift2 (&&) Keyboard.shift isLocked) () Mouse.clicks

port exitPointerLock : Signal ()
port exitPointerLock =
    always () <~ keepIf (any (\x -> x == 27)) [] Keyboard.keysDown

-- Input
inputs : Signal Model.Inputs
inputs =
    let dt = (\t -> t/500) <~ (fps 60)
    in  merge (Model.TimeDelta <~ dt ~ Keyboard.wasd
                                     ~ Keyboard.shift
                                     ~ Keyboard.space)
              (Model.Mouse <~ movement)

-- Fold over input
person : Signal Model.Person
person = foldp Update.step Model.defaultPerson inputs

-- Main
main : Signal Element
main =
    let texture=loadTexture "resources/texture.png"
    in Display.scene <~ Window.dimensions ~ person ~ texture
