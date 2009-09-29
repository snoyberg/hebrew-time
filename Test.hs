import Test.Framework (defaultMain)

import qualified Data.Time.Calendar.Hebrew

main :: IO ()
main = defaultMain
    [ Data.Time.Calendar.Hebrew.testSuite
    ]
