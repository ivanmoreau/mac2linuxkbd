{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>
See README for more info
-}

module Configs where

import Data.Binary (Word16)
import System.Process (CreateProcess(std_in,std_out,std_err), createProcess, proc, shell, StdStream (UseHandle, CreatePipe))
import System.IO (hGetContents, hPutStrLn)
import System.Environment (setEnv)

timeval_size_in_bytes :: Int
timeval_size_in_bytes = (64 + 64) `div` 8

newtype Keycode = Keycode Word16
  deriving (Eq, Ord, Show)

data Modifiers = Modifiers {
  left_shift    :: Bool,
  right_shift   :: Bool,
  left_control  :: Bool,
  right_control :: Bool,
  left_option   :: Bool,
  right_option  :: Bool,
  left_command  :: Bool,
  right_command :: Bool
} deriving (Eq, Show)

data Key = Key {
  keycode   :: Keycode,
  modifiers :: Modifiers
}

data Apps = General | Apps [String]

data Rule = Rule {
  from :: Key,
  to   :: Key,
  apps :: Apps
}

-- ISO ES Keycodes
key_ropt = Keycode 100
key_lopt = Keycode 56
key_lctrl = Keycode 29
key_lcmd = Keycode 125
key_rctrl = Keycode 97
key_rcmd = Keycode 126
key_lshift = Keycode 42
key_rshift = Keycode 54
key_e = Keycode 18
key_a = Keycode 30
key_n = Keycode 49
key_ñ = Keycode 39
key_c = Keycode 46
key_v = Keycode 47
key_x = Keycode 45
key_z = Keycode 44
key_y = Keycode 21
key_f = Keycode 33
key_1 = Keycode 2
key_2 = Keycode 3
key_3 = Keycode 4
key_4 = Keycode 5
key_up = Keycode 103
key_down = Keycode 108
key_home = Keycode 102
key_end = Keycode 107
key_backslash = Keycode 86
key_sqbracketleft = Keycode 26
key_sqbracketright = Keycode 27
key_cubracketleft = Keycode 40
key_cubracketright = Keycode 43

empty_mods = Modifiers {
  left_shift    = False,
  right_shift   = False,
  left_control  = False,
  right_control = False,
  left_option   = False,
  right_option  = False,
  left_command  = False,
  right_command = False
}

-- Rules --

rules :: [Rule]
rules = [
  Rule { -- [General] LOpt + ñ -> ROpt + 4
    from = Key {
      keycode = key_ñ,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_4,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + [ -> ROpt + [
    from = Key {
      keycode = key_sqbracketleft,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_sqbracketleft,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + ] -> ROpt + ]
    from = Key {
      keycode = key_sqbracketright,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_sqbracketright,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + { -> ROpt + {
    from = Key {
      keycode = key_cubracketleft,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_cubracketleft,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + } -> ROpt + }
    from = Key {
      keycode = key_cubracketright,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_cubracketright,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + \ -> ROpt + \
    from = Key {
      keycode = key_backslash,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_backslash,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + @ -> ROpt + @
    from = Key {
      keycode = key_2,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_2,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + # -> ROpt + #
    from = Key {
      keycode = key_3,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_3,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [General] LOpt + 1 -> ROpt + 1
    from = Key {
      keycode = key_1,
      modifiers = empty_mods {
        left_option = True
      }
    }, to = Key {
      keycode = key_1,
      modifiers = empty_mods {
        right_option = True
      }
    }, apps = General
  }, Rule { -- [Terminals] LCmd + c -> LCtrl + LShift + c
    from = Key {
      keycode = key_c,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_c,
      modifiers = empty_mods {
        left_control = True,
        left_shift = True
      }
    }, apps = Apps ["org.wezfurlog.wezterm", "gnome-terminal-server"]
  }, Rule { -- [Terminals] LCmd + v -> LCtrl + LShift + v
    from = Key {
      keycode = key_v,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_v,
      modifiers = empty_mods {
        left_control = True,
        left_shift = True
      }
    }, apps = Apps ["org.wezfurlog.wezterm", "gnome-terminal-server"]
  }, Rule { -- [Chrome] LCmd + n -> LCtrl + n
    from = Key {
      keycode = key_n,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_n,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = Apps ["google-chrome-stable", "google-chrome"]
  }, Rule { -- [Chrome] LCmd + LShift + n -> LCtrl + LShift + n
    from = Key {
      keycode = key_n,
      modifiers = empty_mods {
        left_command = True,
        left_shift = True
      }
    }, to = Key {
      keycode = key_n,
      modifiers = empty_mods {
        left_control = True,
        left_shift = True
      }
    }, apps = Apps ["google-chrome-stable", "google-chrome"]
  }, Rule { -- [Chrome] LCmd + F -> LCtrl + F
    from = Key {
      keycode = key_f,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_f,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = Apps ["google-chrome-stable", "google-chrome"]
  }, Rule { -- [General] LCmd + c -> LCtrl + c
    from = Key {
      keycode = key_c,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_c,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCmd + v -> LCtrl + v
    from = Key {
      keycode = key_v,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_v,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCmd + x -> LCtrl + x
    from = Key {
      keycode = key_x,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_x,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCmd + z -> LCtrl + z
    from = Key {
      keycode = key_z,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_z,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCmd + LShift + z -> LCtrl + y
    from = Key {
      keycode = key_z,
      modifiers = empty_mods {
        left_command = True,
        left_shift = True
      }
    }, to = Key {
      keycode = key_y,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCtrl + A -> Home
    from = Key {
      keycode = key_a,
      modifiers = empty_mods {
        left_control = True
      }
    }, to = Key {
      keycode = key_home,
      modifiers = empty_mods
    }, apps = General
  }, Rule { -- [General] LCtrl + E -> End
    from = Key {
      keycode = key_e,
      modifiers = empty_mods {
        left_control = True
      }
    }, to = Key {
      keycode = key_end,
      modifiers = empty_mods
    }, apps = General
  }, Rule { -- [General] LCmd + a -> LCtrl + a
    from = Key {
      keycode = key_a,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_a,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }, Rule { -- [General] LCmd + s -> LCtrl + s
    from = Key {
      keycode = key_s,
      modifiers = empty_mods {
        left_command = True
      }
    }, to = Key {
      keycode = key_s,
      modifiers = empty_mods {
        left_control = True
      }
    }, apps = General
  }]


-- App name handler --
-- This is system dependent. For now, it works on X11.
-- HOWEVER, implementing get_app_name it's enough to
-- get this working on every Linux system.

get_username :: IO String
get_username = do
  let who = shell "who"
  (_, Just out, _, _) <- createProcess who { std_out = CreatePipe  }
  let s2 = proc "grep" ["-m", "1", "(:0)"]
  (_, Just out2, _, _) <- createProcess s2 
    { std_in = UseHandle out, std_out = CreatePipe }
  let s3 = proc "awk" ["{printf \"%s\", $1;}"]
  (_, Just out3, _, _) <- createProcess s3 
    { std_in = UseHandle out2, std_out = CreatePipe  }
  hGetContents out3

get_app_name :: IO String
get_app_name = do
  uname <- get_username
  -- Set environment variable for display :0
  setEnv "DISPLAY" $ ":0"
  let su = proc "sudo" ["su", uname, "-c", "xprop -id `xprop -root 32x '\\t$0' _NET_ACTIVE_WINDOW | cut -f 2` WM_CLASS | sed 's/.*= //' | sed 's/\\\"\\,/\\\"/' | cut -d ' ' -f1 | sed 's/\"//g' | tr -d '\\n' "]
  (_, Just out, _, _) <- createProcess su { std_out = CreatePipe  }
  hGetContents out
