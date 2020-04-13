namespace MonkeyInterpreter

open FsUnit

module LexerTests =

    open Xunit
    open Lexer

    [<Fact>]
    let ``Test Next Token``() =
        let input = "let five = 5;
            let ten = 10;
            let add = fn(x,y) {
                x+y;
            };
            let result = add(five,ten);"

        let expected =
            [ { Type = TokenType.LET
                Literal = Some "let" },
              { Type = TokenType.IDENT
                Literal = Some "five" },
              { Type = TokenType.ASSIGN
                Literal = Some "=" },
              { Type = TokenType.INT
                Literal = Some "5" },
              { Type = TokenType.SEMICOLON
                Literal = Some ";" },
              { Type = TokenType.LET
                Literal = Some "let" },
              { Type = TokenType.IDENT
                Literal = Some "ten" },
              { Type = TokenType.ASSIGN
                Literal = Some "=" },
              { Type = TokenType.INT
                Literal = Some "10" },
              { Type = TokenType.SEMICOLON
                Literal = Some ";" },
              { Type = TokenType.LET
                Literal = Some "let" },
              { Type = TokenType.IDENT
                Literal = Some "add" },
              { Type = TokenType.ASSIGN
                Literal = Some "=" },
              { Type = TokenType.FUNCTION
                Literal = Some "fn" },
              { Type = TokenType.LPAREN
                Literal = Some "(" },
              { Type = TokenType.IDENT
                Literal = Some "x" },
              { Type = TokenType.COMMA
                Literal = Some "," },
              { Type = TokenType.IDENT
                Literal = Some "y" },
              { Type = TokenType.RPAREN
                Literal = Some ")" },
              { Type = TokenType.LBRACE
                Literal = Some "{" },
              { Type = TokenType.IDENT
                Literal = Some "x" },
              { Type = TokenType.PLUS
                Literal = Some "+" },
              { Type = TokenType.IDENT
                Literal = Some "y" },
              { Type = TokenType.SEMICOLON
                Literal = Some ";" },
              { Type = TokenType.RBRACE
                Literal = Some "}" },
              { Type = TokenType.SEMICOLON
                Literal = Some ";" },
              { Type = TokenType.LET
                Literal = Some "let" },
              { Type = TokenType.IDENT
                Literal = Some "result" },
              { Type = TokenType.ASSIGN
                Literal = Some "=" },
              { Type = TokenType.IDENT
                Literal = Some "add" },
              { Type = TokenType.LPAREN
                Literal = Some "(fn)" },
              { Type = TokenType.IDENT
                Literal = Some "five" },
              { Type = TokenType.COMMA
                Literal = Some "," },
              { Type = TokenType.IDENT
                Literal = Some "ten" },
              { Type = TokenType.RPAREN
                Literal = Some ")" },
              { Type = TokenType.SEMICOLON
                Literal = Some ";" },
              { Type = TokenType.EOF
                Literal = None } ]

        let l = NewLexer(input)

        let rec ExtractNextToken (currentLexer, nextToken) acc =
            let acc' = acc @ [ nextToken ]
            match nextToken with
            | { Type = TokenType.EOF; Literal = None } -> acc'
            | _ -> ExtractNextToken (CurrentLexerWithToken(currentLexer)) acc'

        let result = ExtractNextToken (CurrentLexerWithToken(l)) []

        result |> should equal expected
