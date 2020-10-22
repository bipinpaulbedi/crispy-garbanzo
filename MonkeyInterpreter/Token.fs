namespace MonkeyInterpreter

module Token =

    [<RequireQualifiedAccess>]
    type TokenType =
        | ILLEGAL
        | EOF
        | IDENT
        | INT
        | ASSIGN
        | PLUS
        | MINUS
        | BANG
        | ASTERISK
        | SLASH
        | LT
        | GT
        | EQ
        | NOTEQ
        | COMMA
        | SEMICOLON
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        | FUNCTION
        | LET
        | TRUE
        | FALSE
        | IF
        | ELSE
        | RETURN
        
    type Token =
        { Type: TokenType
          Literal: option<string> }
           
    let LookupIdentifier =
        function
        | "fn" -> TokenType.FUNCTION
        | "let" -> TokenType.LET
        | "true" -> TokenType.TRUE
        | "false" -> TokenType.FALSE
        | "if" -> TokenType.IF
        | "else" -> TokenType.ELSE
        | "return" -> TokenType.RETURN
        | _ -> TokenType.IDENT