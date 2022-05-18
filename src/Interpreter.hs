module Interpreter where 

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)

import AST
import AssembleParser (parseAll)

-- Environment

type ProgramIdx = Int
type MsgStr = String
type VarValue = Int

type ProgramTable = M.Map ProgramIdx Ins
type NameTable = M.Map Id VarValue
type LabelTable = M.Map Lbl ProgramIdx
type Comp = Maybe Ordering
type CallStack = [ProgramIdx]
type Env = (ProgramIdx, NameTable, Comp, MsgStr, CallStack) 

-- Interpreter

interpret :: String -> Maybe String
interpret prog = interpret' initEnv where
  pTab = M.fromList $ zip [0..] $ parseAll prog
  isLabel ins = case ins of 
    Label lbl -> Just lbl
    _ -> Nothing
  lTab = M.fromList [(fromJust . isLabel $ pTab M.! i, i) | i <- [0..M.size pTab-1], isJust . isLabel $ pTab M.! i]
  initEnv = (0, M.empty, Nothing, "", [])
  interpret' (ptr, nTab, comp, mesg, cSt)
    | ptr == M.size pTab = Nothing
    | pTab M.! ptr == End = Just mesg
    | otherwise = interpret' (ptr', nTab', comp', mesg', cSt') where
      -- Recognize name and get its value
      readInt ('-':n) = negate $ readInt n 
      readInt n = read n
      getName :: String -> Name
      getName str | head str `elem` "-0123456789" = Left $ readInt str 
                  | otherwise = Right str
      getValue (Left n) = n
      getValue (Right k) = nTab M.! k
      -- Show Msg
      showM (Left str) = str
      showM (Right n) = show $ getValue n
      -- Current instruction
      ins = pTab M.! ptr
      -- Update environment
      ptr' = case ins of
        Jmp lbl -> lTab M.! lbl
        Jne lbl -> if comp `elem` [Just LT, Just GT] then lTab M.! lbl else ptr + 1
        Je lbl -> if comp == Just EQ then lTab M.! lbl else ptr + 1
        Jge lbl -> if comp `elem` [Just EQ, Just GT] then lTab M.! lbl else ptr + 1
        Jg lbl -> if comp == Just GT then lTab M.! lbl else ptr + 1
        Jle lbl -> if comp `elem` [Just LT, Just EQ] then lTab M.! lbl else ptr + 1
        Jl lbl -> if comp == Just LT then lTab M.! lbl else ptr + 1
        Call lbl -> lTab M.! lbl
        Ret -> head cSt + 1
        _ -> ptr + 1
      nTab' = case ins of
        Mov i n -> M.insert i (getValue n) nTab
        Add i n -> M.insert i ((nTab M.! i) + getValue n) nTab
        Sub i n -> M.insert i ((nTab M.! i) - getValue n) nTab
        Mul i n -> M.insert i ((nTab M.! i) * getValue n) nTab
        Div i n -> M.insert i (div (nTab M.! i) (getValue n)) nTab
        Inc i -> M.insert i ((nTab M.! i) + 1) nTab
        Dec i -> M.insert i ((nTab M.! i) - 1) nTab
        _ -> nTab
      comp' = case ins of
        Cmp n1 n2 -> Just $ compare (getValue n1) (getValue n2) 
        _ -> comp
      mesg' = case ins of
        Msg ms -> mesg ++ concat [showM m | m <- ms]
        _ -> mesg
      cSt' = case ins of 
        Call lbl -> ptr:cSt
        Ret -> tail cSt
        _ -> cSt