{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
import Control.Monad
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Foreign.C.Types
import Foreign.Storable
import Test.QuickCheck
import Test.QuickCheck.Monadic

import FFI.Channels
import FFI.TestCaseFFI

-- |Top-level function that runs all libdill QuickCheck tests.
main :: IO ()
main = do
  quickCheck (withMaxSuccess 10000 prop_Simple)
  quickCheck (withMaxSuccess 10000 prop_Simple2)

-- Non-property test
runTest :: CInt -> IO String
runTest val = do
  ch <- dill_chmake
  case ch of
    Nothing -> return "Failed to get channel"
    Just channel -> do
      hdl <- ffi_go_sender (fst channel) val
      case hdl of
        Nothing -> return "Failed to get handle"
        Just handle -> do
          retVal <- dill_chrecv_int (snd channel)
          case retVal of
            Nothing -> return "Failed to receive value"
            Just r ->
              return $
              if r == val
                then "Got correct value"
                else "Got incorrect value"

-- |Test that dill_chmake always returns a channel.
prop_Simple :: Property
prop_Simple =
  monadicIO $ do
    ch <- run dill_chmake
    assert (isJust ch)

-- |Test that a receiver waits for the sender
prop_Simple2 :: CInt -> Property
prop_Simple2 val =
  monadicIO $ do
    res <- run testProp2
    assert (res == Just True)
  where
    testProp2 :: IO (Maybe Bool)
    testProp2 =
      dill_chmake >>= \(Just ch) ->
        ffi_go_sender (fst ch) val >>= \hdl ->
          dill_chrecv_int (snd ch) >>= \(Just retVal) ->
            if val == retVal
              then return (Just True)
              else return Nothing
