{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
{-# LANGUAGE ForeignFunctionInterface #-}

{-
 FFI for the channels part of libdill.h
 -}
module FFI.Channels
  ( dill_chmake
  ) where

import Control.Monad
import Data.Int
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- int dill_chmake(int chv[2]);
dill_chmake :: IO (Maybe (CInt, CInt))
dill_chmake = do
  ch <- newArray [0, 0]
  pokeElemOff ch 0 0
  pokeElemOff ch 1 0
  -- Create channel endpoint handles and check that it worked
  res <- internal_dill_chmake ch
  if res /= 0
    then return Nothing
    else do
      ep1 <- peekElemOff ch 0
      ep2 <- peekElemOff ch 1
      if ep1 >= 0 && ep2 >= 0
        then return $ Just (ep1, ep2)
        else return Nothing

foreign import ccall "dill_chmake" internal_dill_chmake :: Ptr CInt -> IO CInt
