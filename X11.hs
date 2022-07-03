{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>
See README for more info
-}

module X11 where
import Foreign.C (CString, peekCString)

foreign import ccall "x11_name" x11_name :: IO CString

getName :: IO String
getName = do
  x <- x11_name
  peekCString x

