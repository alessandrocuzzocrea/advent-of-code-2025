import Test.Hspec
import qualified Day01Spec
import qualified Day02Spec
import qualified Day03Spec

main :: IO ()
main = hspec $ do
    describe "Day01" Day01Spec.spec
    describe "Day02" Day02Spec.spec
    describe "Day03" Day03Spec.spec
