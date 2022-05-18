module AssembleParser where

import AST

import Data.Char (isLetter, isDigit)
import Data.List (lines)
import Data.Either (fromRight)
import Prelude hiding (div)
import qualified Prelude as P (div)

import qualified Control.Applicative as A
import Text.Parsec hiding (label)
import Text.Parsec.String (Parser)
tryParsers :: [Parser a] -> Parser a
tryParsers = foldr ((A.<|>) . try) A.empty
      
-- Parser

identifier :: Parser Id
identifier = do
  i <- satisfy (\c -> any ($ c) [isLetter, (== '_')])
  is <- many (satisfy (\c -> any ($ c) [isLetter, isDigit, (== '_')]))
  return $ i:is
  
number :: Parser Int
number = tryParsers [p1, p2] where
  p1 = do
    num <- many1 (satisfy isDigit) 
    return $ read num
  p2 = do
    char '-'
    num <- many1 (satisfy isDigit) 
    return $ negate $ read num
    
name :: Parser Name
name = tryParsers [p1, p2] where
  p1 = do
    n <- number
    return $ Left n
  p2 = do
    i <- identifier
    return $ Right i
    
binary :: String -> (Id -> Name -> Ins) -> Parser Ins
binary str op = do
  string str
  spaces
  i <- identifier
  spaces
  char ','
  spaces
  n <- name
  return $ op i n
  
mov = binary "mov" Mov
add = binary "add" Add
sub = binary "sub" Sub 
mul = binary "mul" Mul 
div = binary "div" Div

unary :: String -> (Id -> Ins) -> Parser Ins
unary str op = do
  string str
  spaces
  i <- identifier
  return $ op i
  
inc = unary "inc" Inc
dec = unary "dec" Dec

label :: Parser Ins
label = do
  i <- identifier
  spaces
  char ':'
  return $ Label i
  
cmp :: Parser Ins
cmp = do
  string "cmp"
  spaces
  n1 <- name
  spaces
  char ','
  spaces
  n2 <- name
  return $ Cmp n1 $ n2
  
jump :: String -> (Lbl -> Ins) -> Parser Ins
jump str op = do
  string str
  spaces
  i <- identifier
  return $ op i
  
jmp = jump "jmp" Jmp
jne = jump "jne" Jne
je = jump "je" Je
jge = jump "jge" Jge
jg = jump "jg" Jg
jle = jump "jle" Jle
jl = jump "jl" Jl

call = jump "call" Call

ret :: Parser Ins
ret = do
  string "ret"
  return Ret

message :: Parser Message
message = tryParsers [p1, p2] where
  p1 = do
    char '\''
    str <- many $ satisfy (/= '\'')
    char '\''
    return $ Left str
  p2 = do
    n <- name
    return $ Right n

msg :: Parser Ins
msg = do
  string "msg"
  spaces
  ms <- msg'
  return $ Msg ms
  where
    msg' = tryParsers [p1, p2] where
      p1 = do
        m <- message
        spaces
        char ','
        spaces
        ms <- msg'
        return $ m:ms
      p2 = do
        m <- message
        return [m]
      
end :: Parser Ins
end = do
  string "end"
  return End
  
emp :: Parser Ins
emp = do
  spaces
  return Emp

-- The main parser

ins :: Parser Ins 
ins = tryParsers [p1, p2] where
  ins' = tryParsers [mov, add, sub, mul, div, inc, dec, label, cmp, jmp, jne, je, jge, jg, jle, jl, call, ret, msg, end, emp]
  p1 = do
    spaces
    i <- ins'
    spaces
    char ';'
    many anyChar
    return i
  p2 = do
    spaces
    i <- ins'
    spaces
    return i

parseAll :: String -> [Ins]
parseAll prog = [fromRight (error "parse error") $ parse ins "instruction" i | i <- lines prog]
