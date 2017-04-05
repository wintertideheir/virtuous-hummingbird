module Main where

import Control.Monad
import System.IO.Error
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

begin :: IO Window
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
      return window
    Nothing -> ioError (userError "GLFW failed to create new window.")

loop :: Window -> IO ()
loop window = do
  closeRequest <- windowShouldClose window
  if closeRequest
    then return ()
    else do
      pollEvents
      swapBuffers window
      glClear GL_COLOR_BUFFER_BIT
      loop window

end :: IO ()
end = terminate

main :: IO ()
main = do
  window <- begin
  loop window
  end
