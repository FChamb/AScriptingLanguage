import System.Exit (exitFailure, exitSuccess)

import Test
import TestConversion
import TestOperations
import TestCompletion
import TestBinaryTree

allTestSuites = [
    runMainTests, 
    runConversionTests,
    runOperationTests,
    runCompletionTests,
    runBinaryTreeTests
    ]

main :: IO ()
main = do
    results <- sequence allTestSuites
    let allSuc = and results

    case allSuc of
        True -> exitSuccess
        False -> exitFailure
