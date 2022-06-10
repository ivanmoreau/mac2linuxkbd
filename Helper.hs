{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>
See README for more info
-}

{-# LANGUAGE BlockArguments, TemplateHaskell, LambdaCase #-}

module Helper where

import Language.Haskell.TH.Syntax (lift, Lift)

import Configs
import Data.Binary (Word16)
import Data.Maybe (fromJust)

modifiers_codes :: [Word16]
modifiers_codes = $([| flip map [
    key_ropt,
    key_lopt,
    key_lctrl,
    key_lcmd,
    key_rctrl,
    key_rcmd,
    key_lshift,
    key_rshift
  ] \case
    Keycode keycode -> keycode
  |])

is_modifier :: Word16 -> Bool
is_modifier code = code `elem` modifiers_codes

get_modifiers :: Modifiers -> [Keycode]
get_modifiers modifiers = map fromJust $ filter (/=Nothing) [
    if right_option  modifiers then Just key_ropt   else Nothing,
    if left_option   modifiers then Just key_lopt   else Nothing,
    if left_control  modifiers then Just key_lctrl  else Nothing,
    if left_command  modifiers then Just key_lcmd   else Nothing,
    if right_control modifiers then Just key_rctrl  else Nothing,
    if right_command modifiers then Just key_rcmd   else Nothing,
    if left_shift    modifiers then Just key_lshift else Nothing,
    if right_shift   modifiers then Just key_rshift else Nothing
  ]

set_modifier :: Modifiers -> Word16 -> Bool -> Modifiers
set_modifier modifiers keycode st = let k = Keycode keycode in
  if k == key_ropt then modifiers { right_option = st }
  else if k == key_lopt then modifiers { left_option = st }
  else if k == key_lctrl then modifiers { left_control = st }
  else if k == key_lcmd then modifiers { left_command = st }
  else if k == key_rctrl then modifiers { right_control = st }
  else if k == key_rcmd then modifiers { right_command = st }
  else if k == key_lshift then modifiers { left_shift = st }
  else if k == key_rshift then modifiers { right_shift = st }
  else modifiers

