namespace MonkeyInterpreter

module ParserTests =
    
    open Xunit
    open Lexer
    open Parser
    
//    [<Theory>]
//    [<InlineData("let x = 5;", "x", 5)>]
//    [<InlineData("let y = true;", "y", true)>]
//    [<InlineData("let foobar = y;", "foobar", "y")>]  
//    let ``Test Let Statement`` input expectedIdentifier expectedValue =
//        NewLexer input
//        |> NewParser
//        |> ParseProgram

