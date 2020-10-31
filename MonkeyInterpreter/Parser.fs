namespace MonkeyInterpreter

module Parser =

    open Lexer
    open Token
    open AST

    [<RequireQualifiedAccess>]
    type PrecedenceType =
        | UNKNOWN = 0
        | LOWEST = 1
        | EQUALS = 2
        | LESSGREATER = 3
        | SUM = 4
        | PRODUCT = 5
        | PREFIX = 6
        | CALL = 7
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
        | _ -> PrecedenceType.LOWEST
        
    
    type PrefixParseFn = delegate of Parser -> Option<INode> * Parser
    and InfixParseFn = delegate of Option<INode> * Parser -> Option<INode> * Parser
    and Parser =
        { Lexer: Lexer
          CurrentToken: Option<Token>
          PeekToken: Option<Token>
          PrefixParseFns: Map<TokenType, PrefixParseFn>
          InfixParseFns: Map<TokenType, InfixParseFn> }
        
    let CurrentPrecedence parser = parser.CurrentToken.Value.Type |> LookupPrecedence

    let NextToken parser =
        let (token, lexer) = NextToken parser.Lexer
        { parser with
              Lexer = lexer
              CurrentToken = parser.PeekToken
              PeekToken = Some token }

    let RegisterPrefix tokenType fn parser =
        { parser with
              PrefixParseFns = parser.PrefixParseFns.Add(tokenType, fn) }

    let RegisterInfix tokenType fn parser =
        { parser with
              InfixParseFns = parser.InfixParseFns.Add(tokenType, fn) }

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
    
    let ExpectPeek tokenType parser =
        match parser.PeekToken with
        | Some t -> match parser |> PeekTokenIs tokenType with
                        | true -> true, parser |> NextToken
                        | _ -> false, parser
        | None -> false, parser
    
    let rec ParseInfExpression exp pre parser =
        match parser.CurrentToken with
            | Some v ->
                if parser |> PeekTokenIs TokenType.SEMICOLON || pre > (parser |> PeekPrecedence) then
                    exp, parser |> NextToken
                else match parser.InfixParseFns.TryFind(v.Type) with
                        | Some inf ->
                            let exp', parser' = inf.Invoke (exp, parser)
                            parser' |> ParseInfExpression exp' pre
                        | _ ->
                                    let parser'' = parser |> NextToken
                                    match parser''.InfixParseFns.TryFind(parser''.CurrentToken.Value.Type) with
                                    | Some inf ->
                                        let exp', parser''' = inf.Invoke (exp, parser'')
                                        parser''' |> ParseInfExpression exp' pre
                                    | _ -> exp, parser |> NextToken
            | _ -> exp, parser
        

    let ParseExpression pre parser =
        let prefix =
            match parser.CurrentToken with
            | Some v ->
                match parser.PrefixParseFns.TryFind(v.Type) with
                    | Some fn -> fn
                    | _ -> failwith "no prefix parse function for %s found" pre
            | _ -> failwith "missing current token -> ParseExpression -> Prefix"
        let exp, parser' = parser |> prefix.Invoke
        parser' |> ParseInfExpression exp pre 
        
        
    let ParseIdentifier parser =
        match parser.CurrentToken with
        | Some v -> (Some({ Token = v; Value = v.Literal.Value.ToString() } :> INode), parser)
        | _ -> None, parser

    let ParsePrefixExpression parser =
        let exp = match parser.CurrentToken with
                    | Some v ->
                            { Token = v
                              Operator = v.Literal.Value
                              Right = Unchecked.defaultof<INode>} 
                    | _ -> failwith "missing current token -> ParsePrefixExpression"
                    
        let (nodeOption, parser') = parser |> NextToken |> ParseExpression PrecedenceType.PREFIX

        match nodeOption with
           | Some np -> Some ({ exp with Right = np } :> INode), parser'
           | _ -> failwith "missing right expression for parse prefix expression"
                                   
    let ParseIntegerLiteral parser =
        match parser.CurrentToken with
        | Some v ->
            Some
                ({ Token = v
                   IntValue =
                       match v.Literal with
                       | Some i -> i |> int64
                       | _ -> failwith "missing integer literal" } :> INode) , parser
        | _ -> None, parser

    let ParseBoolean parser =
        match parser.CurrentToken with
        | Some v ->
            Some
                ({ Token = v
                   BoolValue = parser |> CurrentTokenIs TokenType.TRUE } :> INode), parser
        | _ -> None, parser

    let ParseGroupedExpression parser =
        let nodeOption, parser' = parser
                                    |> NextToken
                                    |> ParseExpression PrecedenceType.LOWEST
        
        let valid, parser'' = parser' |> ExpectPeek TokenType.RPAREN
        
        match valid with
        | true -> nodeOption, parser''
        | false -> let valid', parser''' = parser'' |> ExpectPeek TokenType.EOF
                   match valid' with
                        | true -> nodeOption, parser'''
                        | false -> nodeOption, parser'''
        
    let ParseLetStatement parser =
        let valid, parser' = parser |> ExpectPeek TokenType.IDENT
        let valid', parser'' = match valid with
                                | true -> parser' |> ExpectPeek TokenType.ASSIGN
                                | false -> valid, parser'
        let parser''' = match valid' with
                            | true -> parser'' |> NextToken
                            | false -> parser''
                            
        let stmt, parser'''' = parser''' |> ParseExpression PrecedenceType.LOWEST
        
        Some
            ({ Token = parser.CurrentToken.Value
               Name = { Token = parser'.CurrentToken.Value
                        Value = parser'.CurrentToken.Value.Literal.Value }
               Value = stmt } :> INode), parser'''' |> NextToken
    
    let ParseReturnStatement parser =
        let rv, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
        
        Some ({ Token = parser.CurrentToken.Value
                ReturnValue = rv } :> INode), parser' |> NextToken
    
    let ParseExpressionStatement parser =
        let exp, parser' = parser |> ParseExpression PrecedenceType.LOWEST
        
        Some ({ Token = parser.CurrentToken.Value
                Expression = exp } :> INode), parser' |> NextToken
        
    
    let ParseStatement parser =
        let rtn, parser' = match parser.CurrentToken with
                             | Some t -> match t.Type with
                                            | TokenType.LET -> parser |> ParseLetStatement
                                            | TokenType.RETURN -> parser |>  ParseReturnStatement
                                            | _ -> parser |> ParseExpressionStatement
                             | _ -> None, parser
        
        match parser' |> CurrentTokenIs TokenType.SEMICOLON with
             | true -> rtn, parser' |> NextToken
             | false -> rtn, parser'

    let rec ParseStatements accumulator statement parser =
            let accumulator' = accumulator @ [ statement ]
            match (parser |> CurrentTokenIs TokenType.RBRACE || parser |> CurrentTokenIs TokenType.EOF)  with
                | true -> accumulator', parser
                | _ -> let nextStatement, parser' = parser |> ParseStatement
                       ParseStatements accumulator' nextStatement parser'
    
    let ParseBlockStatement parser =
        let statement, parser' = parser |> NextToken |> ParseStatement
        let Statements, parser'' = ParseStatements [] statement parser'
        match parser.CurrentToken with
                | Some v ->
                    Some
                        ({ Token = v
                           Statements = Statements |> Array.ofList } :> INode), parser'' |> NextToken
                | _ -> failwith "Missing current token -> ParseBlockStatement"

    let ParseIfExpression parser =
        let valid, parser' = parser |> ExpectPeek TokenType.LPAREN
        let con, parser'' = parser' |> NextToken |> ParseExpression PrecedenceType.LOWEST
                
        let parser''' = match parser'' |> ExpectPeek TokenType.RPAREN with
                                | true, p' ->
                                    match p' |> ExpectPeek TokenType.LBRACE with
                                     | _, p'' -> p''
                                | false, p' -> p'
        
        let cons, parser'''' = parser''' |> ParseBlockStatement
        
        let parser''''' = match parser'''' |> PeekTokenIs TokenType.ELSE with
                                | true -> match parser'''' |> ExpectPeek TokenType.LBRACE with
                                                | _, p -> p
                                | false -> parser''''
        
        let alt, parser'''''' = parser''''' |> ParseBlockStatement
        
        let altOption = match alt with
                         | Some a -> Some (a :?> BlockStatement)
                         | _ -> None
        
        Some ({ Token = parser.CurrentToken.Value
                Condition =  con.Value
                Consequence = cons.Value :?> BlockStatement
                Alternative = altOption
               } :> INode), parser''''''
        
    let rec ParseFunctionParametersRec accumulator param parser =
            let accumulator' = accumulator @ [ param ]
            match parser |> PeekTokenIs TokenType.COMMA with
                | true -> let parser' = parser |> NextToken |> NextToken
                          let nextParam = { Token = parser'.CurrentToken.Value
                                            Value = parser'.CurrentToken.Value.Literal.Value }
                          ParseFunctionParametersRec accumulator' nextParam parser'
                | _ -> accumulator' |> Array.ofList, parser
        
    let ParseFunctionParameters parser =
        let parser' = parser |> NextToken
        let prms = Unchecked.defaultof<Identifier[]>
        
        match parser |> PeekTokenIs TokenType.RPAREN with
                    | false -> let firstParam = { Token = parser'.CurrentToken.Value
                                                  Value = parser'.CurrentToken.Value.Literal.Value }
                               ParseFunctionParametersRec [] firstParam parser'
                    | _ -> prms, parser'
       
    let ParseFunctionLiteral parser =
        let _, parser' = parser |> ExpectPeek TokenType.LPAREN
        let prms, parser'' = parser' |> ParseFunctionParameters
        let _, parser''' = parser'' |> ExpectPeek TokenType.LBRACE
        let body, parser'''' = parser''' |> ParseBlockStatement
        Some ({ Token = parser.CurrentToken.Value
                Parameters =  prms
                Body = body.Value :?> BlockStatement
               } :> INode), parser''''
    
    let ParseInfixExpression (exp:Option<INode>) parser =
        let exp', parser' = parser |> NextToken |> ParseExpression (parser |> CurrentPrecedence)
        Some ({ Token = parser.CurrentToken.Value
                Operator = parser.CurrentToken.Value.Literal.Value
                Left = exp.Value
                Right = exp'.Value }:> INode), parser'
    
    let rec ParseCallArgs accumulator (arg:Option<INode>) parser : INode[] * Parser =
        let accumulator' = match arg with
                                | Some a -> accumulator @ [ a ]
                                | _ -> accumulator
        match parser |> CurrentTokenIs TokenType.COMMA with
            | true -> let nextArg, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
                      ParseCallArgs accumulator' nextArg parser'
            | _ -> accumulator' |> Array.ofList , parser
            
    let ParseCallArg parser : INode[] * Parser =
        match parser |> PeekTokenIs TokenType.RPAREN with
            | false -> let arg, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
                       ParseCallArgs [] arg parser'
            | true -> [] |> Array.ofList, parser |> NextToken
    
    let ParseCallExpression (fn:Option<INode>) parser =
        let args, (parser':Parser) = parser |> ParseCallArg
        
        Some ({ Token = parser.CurrentToken.Value
                Function = fn.Value
                Arguments = args }:> INode), parser'
             
    let NewParser (lexer: Lexer): Parser =
        { Lexer = lexer
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
        |> RegisterInfix TokenType.PLUS (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.MINUS (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.SLASH (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.ASTERISK (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.EQ (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.NOTEQ (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.LT (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.GT (InfixParseFn ParseInfixExpression)
        |> RegisterInfix TokenType.LPAREN (InfixParseFn ParseCallExpression)
        |> NextToken
        |> NextToken
    
    let rec ParseProgStatements accumulator stmt parser =
        let accumulator' = match stmt with
                                | Some a -> accumulator @ [ a ]
                                | _ -> accumulator
        match parser |> CurrentTokenIs TokenType.EOF with
            | true -> accumulator', parser
            | _ -> let nextStatement, parser' = parser |> ParseStatement
                   ParseProgStatements accumulator' nextStatement parser'
                           
    let ParseProgram parser =
            let stmts, parser' = parser |> ParseProgStatements [] None 
            { Statements = stmts |> Array.ofList }, parser'
