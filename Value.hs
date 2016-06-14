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
