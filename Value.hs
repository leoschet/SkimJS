module Value (Value (..)) where

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
-----------------------------------
    | Break
-----------------------------------
    | Array [Value]
----------------------------------
    | Nil

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
-----------------------------------
  show Break = "break"
-----------------------------------
  show (Array array) = show array
  show Nil = "undefined"
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

-- compare the arrays to know if are the same

instance Eq Value where
   (Int a) == (Int b) = a == b
   (Array []) == (Array []) = True
   (Array []) == (Array a) = False
   (Array a) == (Array []) = False
   (Array a) == (Array b) = compareArray a b

compareArray [] [] = True
compareArray (x:xs) (y:ys) | show (x) == show (y) = compareArray xs ys
                           | otherwise = False

 