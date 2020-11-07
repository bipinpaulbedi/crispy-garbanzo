namespace MonkeyInterpreter

    open Microsoft.VisualStudio.TestPlatform.TestHost
    open MonkeyInterpreter.AST

    module Repl =
        
        open System
        open Lexer
        open MonkeyEnvironment
        open Parser
        open Evaluator
        
        let rec start() =
             printf "%s" ">> "
             match Console.ReadLine() with
             | "quit" -> Environment.Exit 0
             | command -> let program, _ = NewLexer(command)
                                                    |> NewParser
                                                    |> ParseProgram
                          let e = Eval (NewEnvironment None) (program :> INode)
                          e.Inspect() |> ignore  
             start()
