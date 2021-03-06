{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Data.Int (Int16, Int32)
import SDL
import qualified Data.Vector.Storable.Mutable as V

sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * t)))
      [0 :: Int32 ..]

otherSamples :: [Int16]
otherSamples =
  map (\n ->
         let t = fromIntegral n / 30000 :: Double
             freq = 300
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * t)))
      [0 :: Int32 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         zipWithM_ (V.write buffer)
                   [0 ..]
                   (take n samples')
         writeIORef samples
                    (drop n samples')
    _ -> error "Unsupported audio format"

playSound :: Maybe [Int16] -> IO ()
playSound Nothing = do 
  return()
playSound (Just sampleData) = do 
  samples <- newIORef sampleData
  (device,_) <-
    openAudioDevice
      OpenDeviceSpec {
        SDL.openDeviceFreq      = Mandate 48000
       ,SDL.openDeviceFormat    = Mandate Signed16BitNativeAudio
       ,SDL.openDeviceChannels  = Mandate Mono
       ,SDL.openDeviceSamples   = 4096 * 2
       ,SDL.openDeviceCallback  = audioCB samples
       ,SDL.openDeviceUsage     = ForPlayback
       ,SDL.openDeviceName      = Nothing
      }
  setAudioDevicePlaybackState device Play
  threadDelay 1000000
  closeAudioDevice device

chooseSample :: [EventPayload] -> (Maybe [Int16])
chooseSample [] = Nothing
chooseSample events = head (map eventToSound events)

eventToSound :: EventPayload -> Maybe [Int16]
eventToSound (KeyboardEvent e) 
  | keyboardEventKeyMotion e == Pressed && keysymKeycode (keyboardEventKeysym e) == KeycodeA = Just sinSamples
  | keyboardEventKeyMotion e == Pressed && keysymKeycode (keyboardEventKeysym e) == KeycodeZ = Just otherSamples
  | otherwise = Nothing
eventToSound _ = Nothing

main :: IO ()
main = do 
  initializeAll
  window <- createWindow "Sound player" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window
  quit

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- map eventPayload <$> pollEvents
  let quit = QuitEvent `elem` events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  playSound (chooseSample events)
  unless quit (appLoop renderer)
