module Value (Value (..)) where

-- Importing Syntax to Value Module
import Language.ECMAScript3.Syntax

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
----------------------------------
    | GlobalVar
    | Function Id [Id] [Statement]
    | Return Value 
    | NReturn


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
  show (Function (Id name) args stmts) = "function " ++ name ++ "(" ++ showArgs args ++")"
  

-- Args of Functions
showArgs [] = ""
showArgs ((Id arg):xs) = show arg ++ showArgsTail xs
showArgsTail [] = ""
showArgsTail ((Id arg):xs) = ", " ++ show arg ++ showArgsTail xs

showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

-- compare the arrays to know if are the same

instance Eq Value where
   (Int a) == (Int b) = a == b
   (String []) == (String []) = True
   (String []) == (String a) = False
   (String a) == (String []) = False
   (String a) == (String b) = compareArray a b
   (Array []) == (Array []) = True
   (Array []) == (Array a) = False
   (Array a) == (Array []) = False
   (Array a) == (Array b) = compareArray a b

compareArray [] [] = True
compareArray (x:xs) (y:ys) | show (x) == show (y) = compareArray xs ys
                           | otherwise = False

 
