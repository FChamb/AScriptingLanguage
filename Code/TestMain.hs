import System.Exit (exitFailure, exitSuccess)

import Test
import TestConversion
import TestOperations
import TestCompletion

main :: IO ()
main = do
    results <- sequence [runMainTests, runConversionTests, runOperationTests, runCompletionTests]
    let allSuc = and results

    case allSuc of
        True -> exitSuccess
        False -> exitFailure
