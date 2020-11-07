namespace MonkeyInterpreter
    module Token =

        [<RequireQualifiedAccess>]
        type TokenType =
            | ILLEGAL
            | EOF
            | IDENT
            | INT
            | STRING
            | ASSIGN
            | PLUS
            | MINUS
            | BANG
            | ASTERISK
            | SLASH
            | LT
            | GT
            | EQ
            | NOT_EQ
            | COMMA
            | SEMICOLON
            | COLON
            | LPAREN
            | RPAREN
            | LBRACE
            | RBRACE
            | LBRACKET
            | RBRACKET
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