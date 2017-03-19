module Main where

import Data.Int
import Control.Monad
import System.IO.Error
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL

toInt32 :: Int -> Int32
main :: IO ()

toInt32 x = fromInteger $ toInteger x
main = do
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
      viewport $= (Position 0 0, Size (toInt32 $ fst windowSize) (toInt32 $ snd windowSize))
      loop where
        loop = do
          closeRequest <- windowShouldClose window
          if closeRequest
          then return ()
          else do
            pollEvents
            swapBuffers window
            clear [ColorBuffer]
            loop
    Nothing -> ioError (userError "GLFW failed to create new window.")
  terminate
