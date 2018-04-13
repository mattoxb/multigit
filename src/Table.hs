module Table where

import qualified Control.Monad as M
import           Data.List (intersperse)

data Table =
  Table { columns :: Int
        , entries :: [TablePart]}

data TablePart =
    Row [String]
  | HR


maxLength :: Table -> [Int]
maxLength table = aux (entries table) (take (columns table) (repeat 0))
  where aux []     len = len
        aux ((Row x):xs) len = aux xs $ zipWith max (map length x) len
        aux (HR:xs)      len = aux xs len

lpad :: Int -> String -> String
lpad len str =
  let diff = max 0 (len - length str)
   in (take diff (repeat ' ')) ++ str

showRow :: [Int] -> TablePart -> String
showRow lengths (Row fields) =
  concat $ intersperse " " $ zipWith lpad lengths fields
showRow lengths HR = take (sum lengths + length lengths - 1) (repeat '-')

printTable :: Table -> IO ()
printTable table =
  let lengths = maxLength table
   in M.mapM_ putStrLn (map (showRow lengths) (entries table))

t = Table 4
     [Row ["ID", "Rubric", "Semester", "Name"]
     ,HR
     ,Row ["1","CS 421","2018-01","Programming Languages"]
     ,Row ["2","CS 421","2017-08","Data Structures"]]
