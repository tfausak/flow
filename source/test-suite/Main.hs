import qualified Control.Monad as Monad
import qualified Flow
import qualified System.Exit as Exit
import qualified Test.HUnit as Test

main :: IO ()
main = do
  counts <-
    Test.runTestTT $
      Test.TestList
        [ True Test.~?= True,
          (3 Flow.|> succ Flow.|> recip Flow.|> negate) Test.~?= (-0.25 :: Double),
          (negate Flow.<| recip Flow.<| succ Flow.<| 3) Test.~?= (-0.25 :: Double),
          fmap (Flow.apply 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (2 Flow.|>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (2 Flow.|>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (Flow.<| 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (Flow.apply 3 . Flow.compose succ) [recip, negate]
            Test.~?= [0.25, -4 :: Double],
          (succ Flow..> recip Flow..> negate) 3 Test.~?= (-0.25 :: Double),
          (negate Flow.<. recip Flow.<. succ) 3 Test.~?= (-0.25 :: Double),
          fmap ((\f -> f 3) . (succ Flow..>)) [recip, negate]
            Test.~?= [0.25, -4 :: Double],
          fmap ((\f -> f 3) . (succ Flow..>)) [recip, negate]
            Test.~?= [0.25, -4 :: Double],
          fmap ((\f -> f 3) . (Flow.<. succ)) [recip, negate]
            Test.~?= [0.25, -4 :: Double],
          (3 Flow.!> succ Flow.!> recip Flow.!> negate) Test.~?= (-0.25 :: Double),
          (undefined Flow.|> const True) Test.~?= True,
          (negate Flow.<! recip Flow.<! succ Flow.<! 3) Test.~?= (-0.25 :: Double),
          (const True Flow.<| undefined) Test.~?= True,
          fmap (Flow.apply' 2) [succ, recip, negate]
            Test.~?= [3, 0.5, -2 :: Double],
          Flow.apply undefined (const True) Test.~?= True,
          fmap (2 Flow.!>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (2 Flow.!>) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double],
          fmap (Flow.<! 2) [succ, recip, negate] Test.~?= [3, 0.5, -2 :: Double]
        ]

  let hasErrors = Test.errors counts /= 0
      hasFailures = Test.failures counts /= 0
  Monad.when (hasErrors || hasFailures) Exit.exitFailure
