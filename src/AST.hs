module AST where

-- Instruction definition

type Id = String
type Lbl = Id
type Name = Either Int Id 
type Message = Either String Name
data Ins = Emp 
         | Mov Id Name | Add Id Name | Sub Id Name | Mul Id Name | Div Id Name
         | Inc Id | Dec Id 
         | Label Lbl
         | Cmp Name Name
         | Jmp Lbl | Jne Lbl | Je Lbl | Jge Lbl | Jg Lbl | Jle Lbl | Jl Lbl 
         | Call Lbl | Ret
         | Msg [Message]
         | End
         deriving (Eq, Show)