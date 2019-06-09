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
  , ffi_go_sender2
  ) where

import qualified Control.Exception as CE
import Foreign.C
import Foreign.Ptr

import FFI.Channels

ffi_go_sender :: CInt -> CInt -> IO (Maybe CInt)
ffi_go_sender ch val = do
  hdl <- internal_ffi_go_sender ch val
  if hdl < 0
    then return Nothing
    else return (Just hdl)

foreign import ccall "ffi_go_sender" internal_ffi_go_sender
  :: CInt -> CInt -> IO CInt

type Callback = IO ()

foreign import ccall "go_coroutine" goCoroutine :: FunPtr Callback -> IO ()

foreign import ccall "wrapper" mkCallback :: Callback -> IO (FunPtr Callback)

ffi_go_sender2 :: CInt -> CInt -> IO (Maybe CInt)
ffi_go_sender2 ch val = do
  cbPtr <- mkCallback sender
  goCoroutine cbPtr
  freeHaskellFunPtr cbPtr
  return $ Just val
  where
    sender :: IO ()
    sender = do
      rc <- dill_chsend_int ch val
      CE.assert (rc == 0) $ return ()
