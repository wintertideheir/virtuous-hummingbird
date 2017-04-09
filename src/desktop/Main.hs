module Main where

import Control.Monad
import System.IO.Error
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Foreign.Marshal
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr

data AikaterineState = AikaterineState
  { windowState     :: Window
  , programState    :: GLuint }

createShader :: String -> GLenum -> IO GLuint
createShader source shaderType = do
  shader <- glCreateShader shaderType
  withCString source (\source ->
    with source (\sources ->
      glShaderSource shader 1 sources nullPtr))
  glCompileShader shader
  success <-
    alloca (\success -> do
      glGetShaderiv shader GL_COMPILE_STATUS success
      peek (castPtr success))
  when (success == GL_FALSE) (do
    let
      userErrorMessage =
        case shaderType of
          GL_VERTEX_SHADER ->   "Vertex shader failed to compile."
          GL_FRAGMENT_SHADER -> "Fragment shader failed to compile."
          otherwise ->          "Shader failed to compile."
    allocaArray 512 (\info -> do
      glGetShaderInfoLog shader 512 nullPtr info
      info <- peekCString info
      ioError (userError (userErrorMessage ++
                          (if (info == "") then "" else " ") ++ info))))
  return shader

shaderVertex :: IO GLuint
shaderVertex = createShader
  "#version 330 core\n\
  \layout (location = 0) in vec3 position;\n\
  \void main()\n\
  \{\n\
  \gl_Position = vec4(position, 1.0);\n\
  \}\n"
  GL_VERTEX_SHADER

shaderFragment :: IO GLuint
shaderFragment = createShader
  "#version 330 core\n\
  \out vec4 color;\n\
  \void main()\n\
  \{\n\
  \color = vec4(0.5f, 0.5f, 0.5f, 1.0f);\n\
  \}\n"
  GL_FRAGMENT_SHADER

shaderProgram :: IO GLuint
shaderProgram = do
  program <- glCreateProgram
  vertex <- shaderVertex
  fragment <- shaderFragment
  glAttachShader program vertex
  glAttachShader program fragment
  glLinkProgram program
  success <- alloca (\success -> do
    glGetProgramiv program GL_LINK_STATUS success
    peek (castPtr success))
  when (success == GL_FALSE) (do
    info <- (allocaArray 512 (\info -> do
      glGetProgramInfoLog program 512 nullPtr info
      peekCString info))
    ioError (userError ("Program failed linking.\n" ++
             (if (info == "") then "" else " " ++ info))))
  glDetachShader program vertex
  glDeleteShader vertex
  glDetachShader program fragment
  glDeleteShader fragment
  return program

begin :: IO AikaterineState
begin = do
  glfwInitSuccess <- GLFW.init
  when (not glfwInitSuccess) (ioError (userError "GLFW failed to initialize."))
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'Resizable True)
  newWindow <- createWindow 800 600 "Aikaterine" Nothing Nothing
  case newWindow of
    Just window -> do
      makeContextCurrent newWindow
      windowSize <- getFramebufferSize window
      let toInt32 x = fromInteger $ toInteger x in
        glViewport 0 0 (toInt32 $ fst windowSize) (toInt32 $ snd windowSize)
      program <- shaderProgram
      return (AikaterineState window program)
    Nothing -> ioError (userError "GLFW failed to create new window.")

loop :: AikaterineState -> IO ()
loop aS = do
  closeRequest <- windowShouldClose (windowState aS)
  if closeRequest
    then return ()
    else do
      pollEvents
      swapBuffers (windowState aS)
      glClear GL_COLOR_BUFFER_BIT
      loop aS

end :: IO ()
end = terminate

main :: IO ()
main = do
  aS <- begin
  loop aS
  end
