namespace MonkeyInterpreter

open MonkeyInterpreter.AST

module ASTTests =

    open FsUnit
    open Xunit
    open Token

    [<Fact>]
    let ``Test AST String`` () =
        let input =
            { Statements =
                  [| { Token =
                           { Type = TokenType.LET
                             Literal = Some "let" }
                       Name =
                           { Token =
                                 { Type = TokenType.IDENT
                                   Literal = Some "myVar" }
                             Value = "myVar" }
                       Value =
                           Some
                               ({ Token =
                                      { Type = TokenType.IDENT
                                        Literal = Some "anotherVar" }
                                  Value = "anotherVar" } :> INode) } |] }

        input.ToString()
        |> should equal "let myVar = anotherVar;"
