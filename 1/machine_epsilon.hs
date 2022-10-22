import Control.Monad (liftM2)

machineEpsilon :: Double
machineEpsilon = last $ takeWhile (\x -> 1 + x /= 1)
                      $ drop 1
                      $ iterate (/ 2) 1

machineEpsilon' :: Double
machineEpsilon' = 2 * until ((1 ==) . (1 +)) (/ 2) 1
-- 2 * is optional to be of the same value as machineEpsilon

machineEpsilon'' :: Double
machineEpsilon'' = 2 * until (liftM2 (.) (==) (+) 1) (/ 2) 1
-- 2 * is optional to be of the same value as machineEpsilon

main :: IO ()
main = mapM_ print [ machineEpsilon
                   , machineEpsilon'
                   , machineEpsilon''
                   ]
