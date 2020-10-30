namespace MonkeyInterpreter

open System
open MonkeyInterpreter.AST

module ParserTests =
    
    open FsUnit
    open Xunit
    open Lexer
    open Parser
    open AST
    
    let TestLetStatement exp (inp:INode) name =
        inp.LiteralFromToken() |> should equal "let"
        (inp :?> LetStatement).Name.Value |> should equal exp
        ((inp :?> LetStatement).Name :> INode).LiteralFromToken() |> should equal exp
    
    let TestIntegerLiteral (exp:int64) (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString())
        (inp :?> IntegerLiteral).IntValue |> should equal exp
        
    let TestIdentifier exp (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString())
        (inp :?> Identifier).Value |> should equal exp
    
    let TestBooleanLiteral exp (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString().ToLower())
        (inp :?> Boolean).BoolValue |> should equal exp
    
    let TestLiteralExpression (exp:Object) (inp:Option<INode>) =
        match inp with
            | Some input -> match exp with
                                | :? int as i -> input |> TestIntegerLiteral (int64(i))
                                | :? int64 as i -> input |> TestIntegerLiteral i
                                | :? string as s -> input |> TestIdentifier s
                                | :? bool as b -> input |> TestBooleanLiteral b
                                | _ -> 1 |> should equal 1
            | _ -> 1 |> should equal 1
        
            
    [<Theory>]
    [<InlineData("let x = 5;", "x", 5)>]
    [<InlineData("let y = true;", "y", true)>]
    [<InlineData("let foobar = y;", "foobar", "y")>]  
    let ``Test Let Statement`` inp expIdentifier expValue =
        let p, parser' = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        p.Statements.Length |> should greaterThan 0
        p.Statements.[0] |> TestLetStatement expIdentifier |> ignore
        (p.Statements.[0] :?> LetStatement).Value |> TestLiteralExpression expValue |> ignore

