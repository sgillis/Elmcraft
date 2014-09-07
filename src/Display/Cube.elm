module Display.Cube where

import Model

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

type Vertex = { position:Vec3 
              , normal:Vec3
              , texCoord:Vec3
              }

cube : Texture -> Mat4 -> Model.Block -> Entity
cube texture camera block =
    entity vertexShader fragmentShader cubeMesh
        { camera=camera
        , translation=block.translation
        , light=(vec3 20 20 20) 
        , texture=texture
        }

cubeMesh : [ Triangle Vertex ]
cubeMesh =
    concat
    [ face (rotateFace ( 0,     0     )) (vec3 0.5  1 0)
    , face (rotateFace ( pi/2,  0     )) (vec3 0.0  1 0)
    , face (rotateFace ( -pi/2, 0     )) (vec3 0.25 1 0)
    , face (rotateFace ( 0,     pi    )) (vec3 0.5  1 0)
    , face (rotateFace ( 0,     pi/2  )) (vec3 0.5  1 0)
    , face (rotateFace ( 0,     -pi/2 )) (vec3 0.5  1 0)
    ]

rotateFace : (Float, Float) -> Mat4
rotateFace (anglex, angley) =
    (makeRotate anglex i) `mul` (makeRotate angley j)

face : Mat4 -> Vec3 -> [Triangle Vertex]
face transform topLeftCoord =
  let topLeft     = Vertex (vec3 -0.5  0.5 0.5)
                           (vec3 0 0 1)
                           topLeftCoord
      topRight    = Vertex (vec3  0.5  0.5 0.5)
                           (vec3 0 0 1)
                           (topLeftCoord `add` (vec3 0.25 0 0))
      bottomLeft  = Vertex (vec3 -0.5 -0.5 0.5)
                           (vec3 0 0 1)
                           (topLeftCoord `add` (vec3 0 -0.25 0))
      bottomRight = Vertex (vec3  0.5 -0.5 0.5)
                           (vec3 0 0 1)
                           (topLeftCoord `add` (vec3 0.25 -0.25 0))
  in
      transformMesh
        [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
        transform

transformMesh : [ Triangle Vertex ] -> Mat4 -> [ Triangle Vertex ]
transformMesh mesh t =
    map 
        (mapTriangle (\v -> 
            { v | position <- transform t v.position 
                , normal   <- transform t v.normal }))
        mesh

-- Shaders
vertexShader : Shader Vertex
                      { u | camera:Mat4, translation:Mat4 }
                      { n:Vec3, v:Vec3, vcoord:Vec2 }
vertexShader = [glsl| 

precision mediump float;

attribute vec3 position;
attribute vec3 normal;
attribute vec3 texCoord;

uniform mat4 camera;
uniform mat4 translation;

varying vec3 v;
varying vec3 n;
varying vec2 vcoord;

void main () {
    v = vec3(translation * vec4(position, 1.0));
    n = vec3(vec4(normal, 1.0));
    vcoord = texCoord.xy;
    gl_Position = camera * translation * vec4(position, 1);
}

|]

fragmentShader : Shader {}
                        { u | light:Vec3, texture:Texture }
                        { n:Vec3, v:Vec3, vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;

uniform vec3 light;
uniform sampler2D texture;

varying vec3 v;
varying vec3 n;
varying vec2 vcoord;

void main () {
    vec3 L = normalize(light - v);
    vec3 Idiff = vec3(1.0, 1.0, 1.0) * max(dot(n,L), 0.1);
    Idiff = clamp(Idiff, 0.0, 1.0); 
    vec4 I = vec4(Idiff, 1.0);

    gl_FragColor = I * texture2D(texture, vcoord);
}

|]
