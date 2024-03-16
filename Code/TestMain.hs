import System.Exit (exitFailure, exitSuccess)

import Test
import TestConversion
import TestOperations

main :: IO ()
main = do
    results <- sequence [runMainTests, runConversionTests, runOperationTests]
    let allSuc = and results
    

    case allSuc of
        True -> exitSuccess
        False -> exitFailure
