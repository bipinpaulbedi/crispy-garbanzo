namespace MonkeyInterpreter
    module Repl =
        
        open System
        open Lexer
        let rec start() =
             printf "%s" ">> "
             match Console.ReadLine() with
             | "quit" -> Environment.Exit 0
             | command -> let (token, lexer) = NewLexer(command)
                                               |> NextToken
                          ConvertLexerToTokensRec [] (Some token, lexer)
                          |> printfn "%A";
             
             start()
