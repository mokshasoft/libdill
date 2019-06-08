{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
import qualified Control.Exception as CE
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
  quickCheck (withMaxSuccess 10000 prop_GetChannel)
  quickCheck (withMaxSuccess 10000 prop_ReceiverWaitsForSender)
  quickCheck (withMaxSuccess 10000 prop_SimultaneousSenders)

-- |Print a string and trigger an assert
triggerAssert :: String -> IO ()
triggerAssert str = do
  putStrLn str
  CE.assert False undefined

-- |Test that dill_chmake always returns a channel.
prop_GetChannel :: Property
prop_GetChannel =
  monadicIO $ do
    ch <- run dill_chmake
    assert (isJust ch)

-- |Test that a receiver waits for the sender
prop_ReceiverWaitsForSender :: CInt -> Property
prop_ReceiverWaitsForSender val = monadicIO $ run testProp
  where
    testProp :: IO ()
    testProp = do
      ch <- dill_chmake
      unless (isJust ch) $ triggerAssert "Failed to get channel"
      let channel = fromMaybe (0, 0) ch
      hdl <- ffi_go_sender (fst channel) val
      unless (isJust hdl) $ triggerAssert "Failed to get handle"
      let handle = fromMaybe 0 hdl
      rv <- dill_chrecv_int (snd channel)
      unless (isJust rv) $ triggerAssert "Failed to receive value"
      let retVal = fromMaybe 0 rv
      rc1 <- dill_hclose (snd channel)
      unless (rc1 == 0) $ triggerAssert "Failed to close receiver end-point"
      rc2 <- dill_hclose (fst channel)
      unless (rc2 == 0) $ triggerAssert "Failed to close sender end-point"
      rc3 <- dill_hclose handle
      unless (rc3 == 0) $ triggerAssert "Failed to close sender handle"

-- |Test multiple simultaneous senders, each sender sends one value
prop_SimultaneousSenders :: NonEmptyList CInt -> Property
prop_SimultaneousSenders (NonEmpty vs) = monadicIO $ run testProp
  where
    testProp :: IO ()
    testProp = do
      ch <- dill_chmake
      unless (isJust ch) $ triggerAssert "Failed to get channel"
      let channel = fromMaybe (0, 0) ch
      hdls <- mapM (ffi_go_sender (fst channel)) vs
      unless (all isJust hdls) $ triggerAssert "Failed to get all handles"
      let handles = map (fromMaybe 0) hdls
      rvs <- mapM (\_ -> dill_chrecv_int (snd channel)) handles
      unless (all isJust rvs) $ triggerAssert "Failed to receive all values"
      let retVals = map (fromMaybe 0) rvs
      rc1 <- dill_hclose (snd channel)
      unless (rc1 == 0) $ triggerAssert "Failed to close receiver end-point"
      rc2 <- dill_hclose (fst channel)
      unless (rc2 == 0) $ triggerAssert "Failed to close sender end-point"
      rc3s <- mapM dill_hclose handles
      unless (all (== 0) rc3s) $
        triggerAssert "Failed to close all sender handles"
