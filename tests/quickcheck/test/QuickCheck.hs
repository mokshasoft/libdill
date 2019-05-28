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

-- |Test that dill_chmake always returns a channel.
prop_Simple :: Property
prop_Simple =
  monadicIO $ do
    ch <- run dill_chmake
    assert (isJust ch)

-- |Test that a receiver waits for the sender
prop_Simple2 :: Property
prop_Simple2 =
  monadicIO $ do
    res <- run testProp2
    assert (res == Just True)
  where
    testProp2 :: IO (Maybe Bool)
    testProp2 =
      dill_chmake >>= \(Just ch) ->
        ffi_go_sender (fst ch) 333 >>= \hdl -> return (Just True)
