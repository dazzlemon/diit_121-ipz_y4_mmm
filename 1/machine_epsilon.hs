import Control.Monad (liftM2, ap)

machineEpsilon :: Double
machineEpsilon = last $ takeWhile (\x -> 1 + x /= 1)
                      $ drop 1
                      $ iterate (/ 2) 1

machineEpsilon' :: Double
machineEpsilon' = 2 * until ((1 ==) . (1 +)) (/ 2) 1

machineEpsilon'' :: Double
machineEpsilon'' = 2 * until (liftM2 (.) (==) (+) 1) (/ 2) 1

main :: IO ()
main = do
  print machineEpsilon
  print machineEpsilon'
  print machineEpsilon''
