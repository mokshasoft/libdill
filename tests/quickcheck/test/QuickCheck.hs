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
main = quickCheck (withMaxSuccess 10000 prop_Simple)

-- |Test that dill_chmake always returns a channel.
prop_Simple :: Property
prop_Simple =
  monadicIO $ do
    ch <- run dill_chmake
    assert (isJust ch)
