namespace MonkeyInterpreter

open MonkeyInterpreter.Lexer
open MonkeyInterpreter.Token

module Parser =
    
    [<RequireQualifiedAccess>]
    type PrecedenceType =
        | UNKNOWN
        | LOWEST
        | EQUALS
        | LESSGREATER
        | SUM
        | PRODUCT
        | PREFIX
        | CALL
   
    let LookupPrecedence =
        function
        | TokenType.EQ -> PrecedenceType.EQUALS
        | TokenType.NOTEQ -> PrecedenceType.EQUALS
        | TokenType.LT -> PrecedenceType.LESSGREATER
        | TokenType.GT -> PrecedenceType.LESSGREATER
        | TokenType.PLUS -> PrecedenceType.SUM
        | TokenType.MINUS -> PrecedenceType.SUM
        | TokenType.SLASH -> PrecedenceType.PRODUCT
        | TokenType.ASTERISK -> PrecedenceType.PRODUCT
        | TokenType.LPAREN -> PrecedenceType.CALL
        | _ -> PrecedenceType.UNKNOWN
        
    type Parser =
         { Lexer : Lexer
           Errors: Option<string[]>
           CurrentToken:  Option<Token>
           PeekToken: Option<Token> }
    
    let NextToken parser =
        let (token, lexer) = NextToken parser.Lexer
        { parser with Lexer = lexer; CurrentToken = parser.PeekToken; PeekToken = Some token }
        
        
    let NewParser lexer =
        {
         Lexer = lexer
         Errors = None
         CurrentToken = None
         PeekToken = None }
        |> NextToken |> NextToken
        