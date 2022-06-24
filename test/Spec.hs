import qualified Test.Immutaly.Internal.InMemoryImmutalyProvider as IMIP
import qualified Test.Immutaly.Internal.InMemoryImmutalyProvider.TokenSet as TokenSet
import qualified Test.Immutaly.Internal.Util.Composite as Composite
import qualified Test.Immutaly.Internal.StreamChunk.Builder as Builder
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ IMIP.specs
    , TokenSet.specs
    , Composite.specs
    , Builder.specs
    ]
  defaultMain (testGroup "All Tests" [
      testGroup "Specs" specs
    ])