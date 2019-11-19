-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Spirograph

main :: IO ()
main = do
    test <- testSpec "diagrams-sandbox" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  spirographSpec

spirographSpec :: Spec
spirographSpec =
  describe "spirograph" $ do
    describe "algebraic curve tracing" $ do
      let epsilon = 5e-2
      let shouldVanishOn points eqn = fmap (abs . eqn) points `shouldSatisfy` all (epsilon >) . sort

      it "generates the points of a circle" $ do
        let r_fix = 3
            r_rot = 1
            fix = circle r_fix
            rot = circle r_rot
            points = spirograph' fix rot origin (4*pi) 0.5
        shouldVanishOn points $ \(P (V2 x y)) -> x^2 + y^2 - (r_fix + r_rot)

      it "generates the points of a deltoid" $ do
        let a = 1
            b = 3
            fix = circle b
            rot = circle a
            points = spirograph' fix rot (P $ V2 1 0) (6*pi) 0.5
        shouldVanishOn points $ \(P (V2 x y)) -> (x^2 + y^2)^2 + 18 * a^2 * (x^2 + y^2) - 27 * a^4 - 8 * a * (x^3 - 3 * x * y^2)
