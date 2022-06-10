{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivan@ivmoreau.com>
See README for more info
-}

{-# LANGUAGE BlockArguments, LambdaCase #-}

module Main where

import Data.Binary (Word8, Word16, Word32)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Configs
import Helper
import System.IO (hGetBuf, stdin, stdout, hPutStrLn, stderr, hSetBuffering, BufferMode (NoBuffering))
import GHC.IO.Buffer (newByteBuffer, BufferState (WriteBuffer))
import Data.Bits (Bits(shiftL, shiftR, (.|.)))
import System.Posix (usleep)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Maybe (isNothing)

data InputEvent = InputEvent {
    type_ :: Word16,
    code :: Word16,
    value :: Word32
} deriving (Show)

-- Events
ev_msc :: Word16
ev_msc = 0x04
ev_key :: Word16
ev_key = 0x01
ev_syn :: Word16
ev_syn = 0x00

-- Misc
msc_scan :: Word16
msc_scan = 0x04

-- Two word8 to one word16 (With little endian order convention)
toWord16 :: Word8 -> Word8 -> Word16
toWord16 a b = (fromIntegral a) .|. (fromIntegral b) `shiftL` 8
-- One word16 to two word8 (With little endian order convention)
fromWord16 :: Word16 -> (Word8, Word8)
fromWord16 w = (fromIntegral w, fromIntegral (w `shiftR` 8))
-- Four word8 to one word32 (With little endian order convention)
toWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
toWord32 a b c d = (fromIntegral a) +
  (fromIntegral b) `shiftL` 8 .|.
  (fromIntegral c) `shiftL` 16 .|.
  (fromIntegral d) `shiftL` 24
-- One word32 to four word8 (With little endian order convention)
fromWord32 :: Word32 -> (Word8, Word8, Word8, Word8)
fromWord32 w = (fromIntegral w, 
  fromIntegral (w `shiftR` 8), 
  fromIntegral (w `shiftR` 16), 
  fromIntegral (w `shiftR` 24))

inputevFromByteString :: B.ByteString -> InputEvent
inputevFromByteString bs = InputEvent {
    type_ = type_,
    code = code,
    value = value
}
    where
        initial_index = timeval_size_in_bytes -- timeval is not used
        type_ = toWord16 (B.index bs initial_index) 
          (B.index bs (initial_index + 1))
        code = toWord16 (B.index bs (initial_index + 2))
          (B.index bs (initial_index + 3))
        value = toWord32 (B.index bs (initial_index + 4))
          (B.index bs (initial_index + 5))
          (B.index bs (initial_index + 6))
          (B.index bs (initial_index + 7))

byteStringFromInputev :: InputEvent -> B.ByteString
byteStringFromInputev inputev = B.pack $ [
  -- Timeval zeroed
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  -- Type
  fst type_v, snd type_v,
  -- Code
  fst code_v, snd code_v,
  -- Value
  v1, v2, v3, v4
  ] where
    type_v = fromWord16 $ type_ inputev
    code_v = fromWord16 $ code inputev
    (v1, v2, v3, v4) = fromWord32 $ value inputev

-- Read InputEvent from piped stdin
readInputEvent :: IO InputEvent
readInputEvent = do
  let size = timeval_size_in_bytes + 8 -- timeval is not used + type + code + value
  bs <- B.hGet stdin size
  return $ inputevFromByteString bs

-- Write InputEvent to piped stdout
writeInputEvent :: InputEvent -> IO ()
writeInputEvent inputev = B.hPut stdout $ byteStringFromInputev inputev

-- Write syn event to piped stdout
writeSynEvent :: IO ()
writeSynEvent = do
  B.hPut stdout $ byteStringFromInputev $ InputEvent {
    type_ = ev_syn,
    code = 0,
    value = 0
  }
  -- sleep thread microseconds
  threadDelay 20000

writeKeyCode :: Keycode -> Word32 -> IO ()
writeKeyCode (Keycode k) status = do
  B.hPut stdout $ byteStringFromInputev $ InputEvent {
    type_ = ev_key,
    code = k,
    value = status
  }

-- Write down modifiers to piped stdout
writeModifiersDown :: [Keycode] -> IO ()
writeModifiersDown m = do
  mapM_ (flip writeKeyCode 1) m

-- Write up modifiers to piped stdout
writeModifiersUp :: [Keycode] -> IO ()
writeModifiersUp m = do
  mapM_ (flip writeKeyCode 0) m

-- Write key to piped stdout
writeKey :: Key -> IO ()
writeKey (Key k mods) = do
  let keysmods = get_modifiers mods
  writeModifiersDown keysmods
  writeKeyCode k 0
  writeModifiersUp keysmods
  writeKeyCode k 1
  writeSynEvent

-- Search from rules
searchFromRules :: Keycode -> Modifiers -> [Rule]
searchFromRules k mods = flip filter rules
  \r -> let a = from r in (keycode a) == k && (modifiers a) == mods

-- Compare if there is a rule for the specific app
-- If there is, return the rule
compareApp :: [Rule] -> String -> [Rule]
compareApp rules app = flip filter rules
  \case
    Rule _ _ (Apps xs) ->
      if app `elem` xs then True else False
    _ -> False
compareGeneral :: [Rule] -> [Rule]
compareGeneral rules = flip filter rules
  \case
    Rule _ _ (Apps _) -> False
    _ -> True

andModifiers :: Modifiers -> Modifiers -> Modifiers
andModifiers 
  (Modifiers a b c d e f g h) (Modifiers a' b' c' d' e' f' g' h') =
    Modifiers (a && a') (b && b') (c && c') (d && d') (e && e') 
      (f && f') (g && g') (h && h')

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xorModifiers :: Modifiers -> Modifiers -> Modifiers
xorModifiers 
  (Modifiers a b c d e f g h) (Modifiers a' b' c' d' e' f' g' h') =
    Modifiers (a `xor` a') (b `xor` b') (c `xor` c') (d `xor` d') 
      (e `xor` e') (f `xor` f') (g `xor` g') (h `xor` h')

-- Handle key matching
handleKey :: Keycode -> Modifiers -> IO (Maybe Rule)
handleKey k mods = do
  currentApp <- get_app_name -- get current app name
  let rules = searchFromRules k mods
  if null rules then do
    return Nothing
  else let r = compareApp rules currentApp in if not $ null r then do
    return $ Just $ head r
  else let t = compareGeneral rules in if not $ null t then do
    return $ Just $ head t
  else return Nothing

type Mapping = M.Map Word16 Key

-- Dispach key
-- We need to check what modifiers are going to be changed.
-- So we only perform the change on those that don't match.
dispatchKey :: Key -> Modifiers -> IO ()
dispatchKey (Key k mods) old_mods = do
  let xored = xorModifiers old_mods mods
  let lxored = get_modifiers xored
  when (not $ null lxored) do
    let upmods = get_modifiers $ andModifiers xored old_mods
    let downmods = get_modifiers$ andModifiers xored mods
    writeModifiersUp upmods
    writeModifiersDown downmods
    writeSynEvent
  writeKeyCode k 1

-- Reset modifiers
resetModifiers :: Modifiers -> Modifiers -> IO ()
resetModifiers old_mods mods = do
  let xored = xorModifiers old_mods mods
  let lxored = get_modifiers xored
  when (not $ null lxored) do
    let upmods = get_modifiers $ andModifiers xored old_mods
    let downmods = get_modifiers$ andModifiers xored mods
    writeModifiersUp upmods
    writeModifiersDown downmods
    writeSynEvent

-- Handler
handler :: Modifiers -> Mapping -> IO ()
handler status maps = do
  inputev <- readInputEvent
  if type_ inputev == ev_msc && code inputev == msc_scan then
    handler status maps
  else if type_ inputev /= ev_key then do
    writeInputEvent inputev
    handler status maps
  else let v = value inputev in if v == 2 then
    handler status maps
  else do
    let c = code inputev
    let ns = if is_modifier c then
          set_modifier status c $ v == 1
        else
          status
    if v == 1 then do -- key down
      mapping <- handleKey (Keycode c) ns
      case mapping of
        Just (Rule _ m _) -> do -- We handle the mapping here
          dispatchKey m ns
          let res = M.insert c m maps
          handler ns res
        _ -> return () -- Nothing changed
    else do -- Key up
      let m = M.lookup c maps
      case m of -- Do we have a mapping for this key?
        Just (Key k m') -> do
          writeKeyCode k 0
          let m1 = M.delete c maps
          resetModifiers m' ns
          writeSynEvent
          handler ns m1
        _ -> pure ()
    when (not $ is_modifier c) do
      resetModifiers status ns
    writeInputEvent inputev
    handler ns maps

      
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering 
  hSetBuffering stdout NoBuffering
  handler empty_mods M.empty
