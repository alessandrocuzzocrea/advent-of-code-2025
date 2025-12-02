import Test.Hspec
import qualified Day01Spec
import qualified Day02Spec

main :: IO ()
main = hspec $ do
    describe "Day01" Day01Spec.spec
    describe "Day02" Day02Spec.spec
