namespace MonkeyInterpreter

module Lexer =

    [<RequireQualifiedAccess>]
    type TokenType =
        | ILLEGAL
        | EOF
        | IDENT
        | INT
        | ASSIGN
        | PLUS
        | COMMA
        | SEMICOLON
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        | FUNCTION
        | LET


    type Token =
        { Type: TokenType
          Literal: Option<string> }

    type Lexer =
        { Input: string
          Position: int
          ReadPosition: int
          Ch: Option<char> }

    let ReadChar l =
        match l with
        | { ReadPosition = pos } when pos >= l.Input.Length ->
            { Input = l.Input
              Position = l.ReadPosition
              ReadPosition = l.ReadPosition + 1
              Ch = Option.None }
        | _ ->
            { Input = l.Input
              Position = l.ReadPosition
              ReadPosition = l.ReadPosition + 1
              Ch = Some l.Input.[l.ReadPosition] }

    let NewLexer(inp) =
        { Input = inp
          Position = 0
          ReadPosition = 0
          Ch = Option.None }
        |> ReadChar

    let KeywordLookup =
        function
        | "fn" -> TokenType.FUNCTION
        | "let" -> TokenType.LET
        | _ -> TokenType.IDENT

    let NextToken(l: Lexer) =
        match l.Ch with
        | Some '=' ->
            { Type = TokenType.ASSIGN
              Literal = Some "=" }
        | Some ';' ->
            { Type = TokenType.SEMICOLON
              Literal = Some ";" }
        | Some '(' ->
            { Type = TokenType.LPAREN
              Literal = Some "(" }
        | Some ')' ->
            { Type = TokenType.RPAREN
              Literal = Some ")" }
        | Some ',' ->
            { Type = TokenType.COMMA
              Literal = Some "," }
        | Some '+' ->
            { Type = TokenType.PLUS
              Literal = Some "+" }
        | Some '{' ->
            { Type = TokenType.LBRACE
              Literal = Some "{" }
        | Some '}' ->
            { Type = TokenType.RBRACE
              Literal = Some "}" }
        | _ ->
            { Type = TokenType.EOF
              Literal = None }

    let CurrentLexerWithToken l = (ReadChar(l), NextToken(l))
