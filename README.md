# Assemble-Interpreter
This is an Haskell inplementation of [Assembler interpreter (part II) - CodeWars](https://www.codewars.com/kata/58e61f3d8ff24f774400002c), which is a simple interpreter that supports an assemble-style language, mainly based on Text.Parsec. 

To run the assemble scripts in GHCi:

```haskell
Prelude> :l Interface.hs 
[1 of 4] Compiling AST              ( AST.hs, interpreted )
[2 of 4] Compiling AssembleParser   ( AssembleParser.hs, interpreted )
[3 of 4] Compiling Interpreter      ( Interpreter.hs, interpreted )
[4 of 4] Compiling Interface        ( Interface.hs, interpreted )
Ok, four modules loaded.
*Interface> :run main "../assembly_scripts/calc.a"
(5+1)/2 = 3
```

