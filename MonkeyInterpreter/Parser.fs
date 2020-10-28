namespace MonkeyInterpreter

open System
open System.Collections.Generic
open MonkeyInterpreter

module Parser =
    
    open Lexer
    open Token
    open AST

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
    
    type PrefixParseFn =
        delegate of Parser -> Option<INode>
    and InfixParseFn =
        delegate of Option<INode> * Parser -> Option<INode>
    and  Parser =
         { Lexer : Lexer
           CurrentToken:  Option<Token>
           PeekToken: Option<Token>
           PrefixParseFns: Map<TokenType, PrefixParseFn>
           InfixParseFns:  Map<TokenType, InfixParseFn> }
         
    let NextToken parser =
        let (token, lexer) = NextToken parser.Lexer
        { parser with Lexer = lexer; CurrentToken = parser.PeekToken; PeekToken = Some token }
    
    let RegisterPrefix tokenType fn parser =
        { parser with PrefixParseFns = parser.PrefixParseFns.Add(tokenType, fn) }
        
    let RegisterInfix tokenType fn parser =
        { parser with InfixParseFns = parser.InfixParseFns.Add(tokenType, fn) }
        
    let PeekTokenIs tokenType parser =
        match parser.PeekToken with
        | Some s when s.Type = tokenType -> true
        | _ -> false
    
    let CurrentTokenIs tokenType parser =
        match parser.CurrentToken with
        | Some s when s.Type = tokenType -> true
        | _ -> false
    
    let PeekPrecedence parser =
        match parser.PeekToken with
            | Some t -> LookupPrecedence t.Type
            | None -> PrecedenceType.LOWEST
        
    let ParseExpression pre parser=
        let prefix =
            match parser.CurrentToken with
            | Some v -> match parser.PrefixParseFns.TryFind v.Type with
                        | Some v -> v
                        | _ -> failwith "no prefix parse function for %s found" pre
            | _ -> failwith "missing current token -> ParseExpression -> Prefix"
        
        let mutable exp = parser |> prefix.Invoke
        
        if (parser |> PeekTokenIs TokenType.SEMICOLON) && pre < (parser |> PeekPrecedence)
        then
             exp <- match parser.CurrentToken with
                            | Some v -> match parser.InfixParseFns.TryFind v.Type with
                                            | Some v ->  v.Invoke(exp, parser |> NextToken)
                                            | _ -> exp
                            | _ -> failwith "missing current token -> ParseExpression -> Infix"
        exp
        
    let ParseIdentifier parser =
        match parser.CurrentToken with
        | Some v ->
           Some ({ Token = v
                   Value = v.ToString() } :> INode)
        | _ -> None
    
    let ParsePrefixExpression parser =
        match parser.CurrentToken with
        | Some v ->
           Some ({ Token = v
                   Operator = v.ToString()
                   Right = match parser |> ParseExpression PrecedenceType.LOWEST with
                                | Some v -> v
                                | _ -> failwith "missing right expression for parse prefix expression"} :> INode)
        | _ -> None
    
    let ParseIntegerLiteral parser =
        match parser.CurrentToken with
        | Some v ->
           Some ({ Token = v
                   IntValue = match v.Literal with
                                | Some i -> i |> int64
                                | _ -> failwith "missing integer literal"} :> INode)
        | _ -> None
    
    let ParseBoolean parser =
        match parser.CurrentToken with
        | Some v ->
           Some ({ Token = v
                   BoolValue = parser |> CurrentTokenIs TokenType.TRUE} :> INode)
        | _ -> None
    
    let ParseGroupedExpression parser =
        match parser |> PeekTokenIs TokenType.RPAREN with
            | true -> parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
            | false -> None
        
    let NewParser (lexer: Lexer): Parser =
        {
         Lexer = lexer
         CurrentToken = None
         PeekToken = None
         PrefixParseFns = Map.empty
         InfixParseFns = Map.empty }
        |> RegisterPrefix TokenType.IDENT (PrefixParseFn ParseIdentifier)
        |> RegisterPrefix TokenType.INT (PrefixParseFn ParseIntegerLiteral)
        |> RegisterPrefix TokenType.BANG (PrefixParseFn ParsePrefixExpression)
        |> RegisterPrefix TokenType.MINUS (PrefixParseFn ParsePrefixExpression)
        |> RegisterPrefix TokenType.TRUE (PrefixParseFn ParseBoolean)
        |> RegisterPrefix TokenType.FALSE (PrefixParseFn ParseBoolean)
        |> RegisterPrefix TokenType.LPAREN (PrefixParseFn ParseGroupedExpression)
        |> RegisterPrefix TokenType.IF (PrefixParseFn ParseIfExpression)
        |> RegisterPrefix TokenType.FUNCTION (PrefixParseFn ParseFunctionLiteral)
        |> RegisterInfix TokenType.PLUS  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.MINUS  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.SLASH  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.ASTERISK  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.EQ  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.NOTEQ  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.LT  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.GT  (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.LPAREN  (InfixParseFn ParseCallExpression)
        |> NextToken |> NextToken
        