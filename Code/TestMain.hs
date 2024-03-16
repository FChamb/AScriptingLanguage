import System.Exit (exitFailure, exitSuccess)

import Test


main :: IO ()
main = do
    suc <- runTests
    case suc of
        True -> exitSuccess
        False -> exitFailure
