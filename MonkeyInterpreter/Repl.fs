namespace MonkeyInterpreter

open System
open MonkeyInterpreter

module Repl =
    
    open Lexer
    let rec start() =
         printf "%s" ">> "
         match Console.ReadLine() with
         | "quit" -> Environment.Exit 0
         | command -> let (token, lexer) = NewLexer(command)
                                           |> NextToken
                      ConvertLexerToTokens [] (Some token, lexer)
                      |> printfn "%A";
         
         start()
