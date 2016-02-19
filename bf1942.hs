-- A transliteration of assembly from msvc6 generated executable to Haskell.
-- Credit to OllyDbg (http://www.ollydbg.de) for binary code analysis. 

import Char
import Random
import Text.Printf
import System.IO

genkey :: String -> String
genkey inp = inp ++ (fmt first_number) ++ (fmt last_number)
    where last_number = compute last_sequences magic_1
          last_sequences = (map (\x -> read x :: Int) (sp (inp ++ "00000") 4 5 []))
          magic_1 = 0x43
          first_number = compute first_sequences magic_2
          first_sequences = (map (\x -> read x :: Int) (sp ("00000" ++ inp ++ "00000") 4 7 []))
          magic_2 = 0x25

compute  :: [Int] -> Int -> Int
compute xs m = case xs of
              k1:k2:k3:k4:[] ->  (k4 + (k3 + (k2 + (k1 `mod` m)) `mod` m) `mod` m) `mod` m

fmt v = printf "%02u" (v::Int) :: String

-- split a string into i parts n times
sp :: String -> Int -> Int -> [String] -> [String]
sp xs 0 n acc = acc
sp xs i n acc = sp (drop n xs) (i-1) n (acc ++ [(take n xs)])

main = do
  num <- randomRIO ((10^17 :: Int),(10^18-1 :: Int))
  putStrLn $ genkey $ show num
