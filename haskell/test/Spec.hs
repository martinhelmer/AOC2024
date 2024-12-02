import Test.Hspec

import qualified BSArraySpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BSArray"     BSArraySpec.spec
