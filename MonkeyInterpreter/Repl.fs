namespace MonkeyInterpreter
    module Repl =
        
        open System
        open Lexer
        open MonkeyObject
        open Parser
        open Evaluator
        open AST
        
        let rec start(env: MonkeyEnvironment) =
             printf "%s" ">> "
             match Console.ReadLine() with
             | "quit" -> Environment.Exit 0
             | command -> let program, _ = NewLexer(command)
                                                    |> NewParser
                                                    |> ParseProgram
                          let e = Eval env (program :> INode)
                          e.Inspect() |> ignore  
             start(env)
