namespace MonkeyInterpreter

open System
open MonkeyInterpreter

module Repl =
    
    open Lexer
    let rec start() =
         printf "%s" ">> "
         match Console.ReadLine() with
         | "quit" -> Environment.Exit 0
         | command -> NewLexer(command)
                   |> NextToken
                   |> ProcessLexerToTokens []
                   |> printfn "%A";
         
         start()
