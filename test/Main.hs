module Main(
    main
) where

    import           Test.HUnit

    allTests :: Test
    allTests = TestLabel "All Tests" $
                TestList [
                ]

    main :: IO ()
    main = runTestTTAndExit allTests

