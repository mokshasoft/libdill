{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
{-# LANGUAGE ForeignFunctionInterface #-}

{-
 FFI used by the test cases to handle go and coroutine macros
 -}
module FFI.TestCaseFFI
  ( ffi_go_sender
  ) where

import Foreign.C

foreign import ccall "ffi_go_sender" ffi_go_sender :: CInt -> CInt -> IO CInt
