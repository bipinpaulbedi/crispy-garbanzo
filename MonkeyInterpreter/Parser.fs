namespace MonkeyInterpreter
    module Parser =

        open Lexer
        open System
        open System.Collections.Generic
        open Token
        open AST

        [<RequireQualifiedAccess>]
        type PrecedenceType =
            | UNKNOWN = 0
            | LOWEST = 1
            | EQUALS = 2
            | LESS_GREATER = 3
            | SUM = 4
            | PRODUCT = 5
            | PREFIX = 6
            | CALL = 7
            | INDEX = 8
            
        let LookupPrecedence =
            function
            | TokenType.EQ -> PrecedenceType.EQUALS
            | TokenType.NOT_EQ -> PrecedenceType.EQUALS
            | TokenType.LT -> PrecedenceType.LESS_GREATER
            | TokenType.GT -> PrecedenceType.LESS_GREATER
            | TokenType.PLUS -> PrecedenceType.SUM
            | TokenType.MINUS -> PrecedenceType.SUM
            | TokenType.SLASH -> PrecedenceType.PRODUCT
            | TokenType.ASTERISK -> PrecedenceType.PRODUCT
            | TokenType.LPAREN -> PrecedenceType.CALL
            | TokenType.LBRACKET -> PrecedenceType.INDEX
            | _ -> PrecedenceType.LOWEST
            
        
        type PrefixParseFn = delegate of Parser -> Expression * Parser
        and InfixParseFn = delegate of Expression * Parser -> Expression * Parser
        and Parser =
            { Lexer: Lexer
              Error: string list
              CurrentToken: Token
              PeekToken: Token
              PrefixParseFns: Map<TokenType, PrefixParseFn>
              InfixParseFns: Map<TokenType, InfixParseFn> }
            
        let private CurrentPrecedence parser = parser.CurrentToken.Type |> LookupPrecedence

        let private NextToken parser =
            let (token, lexer) = parser.Lexer |> NextToken 
            { parser with
                  Lexer = lexer
                  CurrentToken = parser.PeekToken
                  PeekToken = token }

        let private RegisterPrefix tokenType fn parser =
            { parser with
                  PrefixParseFns = parser.PrefixParseFns.Add(tokenType, fn) }

        let private RegisterInfix tokenType fn parser =
            { parser with
                  InfixParseFns = parser.InfixParseFns.Add(tokenType, fn) }

        let private PeekTokenIs tokenType parser = parser.PeekToken.Type = tokenType

        let private CurrentTokenIs tokenType parser = parser.CurrentToken.Type = tokenType

        let private PeekPrecedence parser = parser.PeekToken.Type |> LookupPrecedence
        
        let private ExpectPeek tokenType parser =
            match parser |> PeekTokenIs tokenType with
                | true -> true, parser |> NextToken
                | _ -> false, { parser with
                                    Error = parser.Error @ [ String.Format("Expected next token to be {0}, got {1} instead",
                                                                          tokenType, parser.PeekToken.Type ) ] }
        
        let private NoPrefixParseFunctionError (tokenType:TokenType) parser =
            { parser with
                Error = parser.Error @ [ String.Format("No prefix parse function for {0} found",
                                                                          tokenType)]}

        
        let rec private ParseInfExpressionRec exp precedence parser =
                    match parser |> PeekTokenIs TokenType.SEMICOLON || precedence >= (parser |> PeekPrecedence) with
                    | true -> exp, parser
                    | _ ->  match parser.InfixParseFns.TryFind(parser.CurrentToken.Type) with
                                    | Some inf -> let exp', parser' = inf.Invoke (exp, parser |> NextToken)
                                                  parser' |> ParseInfExpressionRec exp' precedence
                                    | _ -> exp, parser

        let private ParseExpression precedence parser =
            match parser.PrefixParseFns.TryFind(parser.CurrentToken.Type) with
                | Some fn -> let exp, parser' = parser |> fn.Invoke
                             parser' |> ParseInfExpressionRec exp precedence
                | _ -> Unchecked.defaultof<Expression>, parser |> NoPrefixParseFunctionError parser.CurrentToken.Type
            
        let private ParseIdentifier parser =
            { Token = parser.CurrentToken; Value = parser.CurrentToken.Literal |> Option.defaultValue "" } :> INode, parser

        let private ParsePrefixExpression parser =
            let exp = { Token = parser.CurrentToken
                        Operator = parser.CurrentToken.Literal |> Option.defaultValue ""
                        Right = Unchecked.defaultof<Expression>} 
            let (rightExpression, parser') = parser |> NextToken |> ParseExpression PrecedenceType.PREFIX
            { exp with Right = rightExpression } :> INode, parser'
                                       
        let private ParseIntegerLiteral parser =
             { Token = parser.CurrentToken
               IntValue = match parser.CurrentToken.Literal with
                            | Some i -> i |> int64
                            | _ -> failwith "missing integer literal" } :> INode , parser

        let private ParseBoolean parser = { Token = parser.CurrentToken
                                            BoolValue = parser |> CurrentTokenIs TokenType.TRUE } :> INode, parser


        let private ParseGroupedExpression parser =
            let expression, parser' = parser
                                        |> NextToken
                                        |> ParseExpression PrecedenceType.LOWEST
            
            let valid, parser'' = parser' |> ExpectPeek TokenType.RPAREN
            
            match valid with
            | true -> expression, parser''
            | false -> Unchecked.defaultof<Expression>, parser''
            
        let private ParseLetStatement parser =
            let valid, parser' = parser |> ExpectPeek TokenType.IDENT
            
            let valid', parser'' = match valid with
                                    | true -> parser' |> ExpectPeek TokenType.ASSIGN
                                    | false -> valid, parser'
                                    
            let parser''' = match valid' with
                                | true -> parser'' |> NextToken
                                | false -> parser''
                                
            let stmt, parser'''' = parser''' |> ParseExpression PrecedenceType.LOWEST
            
            
            { Token = parser.CurrentToken
              Name = { Token = parser'.CurrentToken
                       Value = parser'.CurrentToken.Literal |> Option.defaultValue "" }
              Value = Some stmt } :> INode, parser''''
        
        let private ParseReturnStatement parser =
            let returnValue, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
            
            { Token = parser.CurrentToken
              ReturnValue = Some returnValue } :> INode, parser'
        
        let private ParseExpressionStatement parser =
            let exp, parser' = parser |> ParseExpression PrecedenceType.LOWEST
            
            { Token = parser.CurrentToken
              Expression = Some exp } :> INode, parser'
        
        let private ParseStatement parser =
            let rtn, parser' = match parser.CurrentToken.Type with
                                    | TokenType.LET -> parser |> ParseLetStatement
                                    | TokenType.RETURN -> parser |>  ParseReturnStatement
                                    | _ -> parser |> ParseExpressionStatement
            
            match parser' |> CurrentTokenIs TokenType.SEMICOLON with
                 | true -> rtn, parser' |> NextToken
                 | false -> rtn, parser'

        let rec private ParseBlockStatementRec accumulator statement parser =
                let accumulator' = match statement with
                                    | Some stmt -> accumulator @ [ stmt ]
                                    | None -> accumulator
                match (parser |> CurrentTokenIs TokenType.RBRACE || parser |> CurrentTokenIs TokenType.EOF)  with
                    | true -> accumulator', parser
                    | _ -> let nextStatement, parser' = parser |> ParseStatement
                           ParseBlockStatementRec accumulator' (Some nextStatement) (parser' |> NextToken)
        
        let private ParseBlockStatement parser =
            let Statements, parser'' = ParseBlockStatementRec [] None (parser |> NextToken)
            { Token = parser.CurrentToken
              Statements = Statements |> Array.ofList } :> INode, parser''

        let private ParseIfExpression parser =
            let valid, parser' = parser |> ExpectPeek TokenType.LPAREN
            
            let con, parser'' = match valid with
                                    | true -> parser' |> NextToken |> ParseExpression PrecedenceType.LOWEST
                                    | _ -> Unchecked.defaultof<Expression>, parser'
                    
            let cons, parser''' = match parser'' |> ExpectPeek TokenType.RPAREN with
                                    | true, p' ->
                                        match p' |> ExpectPeek TokenType.LBRACE with
                                         | true, p'' -> p'' |> ParseBlockStatement
                                         | false, p'' -> Unchecked.defaultof<Expression>, p''
                                    | false, p' -> Unchecked.defaultof<Expression>, p'
            
            
            let alt, parser'''' = match parser''' |> CurrentTokenIs TokenType.ELSE with
                                    | true -> let p''' = parser''' |> NextToken
                                              match p''' |> ExpectPeek TokenType.LBRACE with
                                                | true, p'''' -> p'''' |> ParseBlockStatement
                                                | false, p'''' -> Unchecked.defaultof<Expression>, p''''
                                    | false -> Unchecked.defaultof<Expression>, parser'''
            
            
            { Token = parser.CurrentToken
              Condition =  con
              Consequence = cons :?> BlockStatement
              Alternative = Some (alt :?> BlockStatement)
            } :> INode, parser''''
            
        let rec private ParseFunctionParametersRec accumulator param parser =
                let accumulator' = accumulator @ [ param ]
                match parser |> PeekTokenIs TokenType.COMMA with
                    | true -> let parser' = parser |> NextToken |> NextToken
                              let nextParam = { Token = parser'.CurrentToken
                                                Value = parser'.CurrentToken.Literal |> Option.defaultValue "" }
                              ParseFunctionParametersRec accumulator' nextParam parser'
                    | _ -> match parser |> ExpectPeek(TokenType.RPAREN) with
                            | true, parser'' -> accumulator' |> Array.ofList, parser''
                            | false, parser'' -> Unchecked.defaultof<Identifier[]>, parser''
            
        let private ParseFunctionParameters parser =
            let parser' = parser |> NextToken
            match parser |> PeekTokenIs TokenType.RPAREN with
                        | false -> let firstParam = { Token = parser'.CurrentToken
                                                      Value = parser'.CurrentToken.Literal |> Option.defaultValue "" }
                                   ParseFunctionParametersRec [] firstParam parser'
                        | _ -> Unchecked.defaultof<Identifier[]>, parser'
           
        let private ParseFunctionLiteral parser =
            let valid, parser' = parser |> ExpectPeek TokenType.LPAREN
            let fnParams, parser'' = match valid with
                                        | true -> parser' |> ParseFunctionParameters
                                        | false -> Unchecked.defaultof<Identifier[]>, parser'
            let valid', parser''' = parser'' |> ExpectPeek TokenType.LBRACE
            let body, parser'''' = match valid with
                                        | true -> parser''' |> ParseBlockStatement
                                        | false -> Unchecked.defaultof<INode>, parser'''
            
            { Token = parser.CurrentToken
              Parameters =  fnParams
              Body = body :?> BlockStatement
            } :> INode, parser''''
        
        let private ParseInfixExpression (exp:Expression) parser =
            let exp', parser' = parser |> NextToken |> ParseExpression (parser |> CurrentPrecedence)
            { Token = parser.CurrentToken
              Operator = parser.CurrentToken.Literal |> Option.defaultValue ""
              Left = exp
              Right = exp' }:> INode, parser'
            
        let rec private ParseCallExpressionListRec accumulator expression endToken parser =
            let accumulator' = accumulator @ [ expression ]
            match parser |> PeekTokenIs TokenType.COMMA with
                | true -> let exp, parser' = parser |> NextToken |> NextToken |> ParseExpression PrecedenceType.LOWEST
                          ParseCallExpressionListRec accumulator' exp endToken parser'
                | false -> match parser |> ExpectPeek endToken with
                                | true, parser'' -> accumulator' |> Array.ofList, parser''
                                | false, parser'' -> Unchecked.defaultof<Expression[]>, parser''
                    
        
        let private ParseCallExpressionList endToken parser =
             match parser |> PeekTokenIs endToken with
                | true -> Unchecked.defaultof<Expression[]>, parser |> NextToken
                | false -> let exp, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
                           ParseCallExpressionListRec [] exp endToken parser'
        
        let private ParseCallExpression (fn:Expression) parser =
            let args, parser' = parser |> ParseCallExpressionList TokenType.RPAREN
            
            { Token = parser.CurrentToken
              Function = fn
              Arguments = args }:> INode, parser'
        
        let private ParseStringLiteral parser =
            { Token = parser.CurrentToken
              StringValue = parser.CurrentToken.Literal |> Option.defaultValue "" }:> INode, parser
        
        let private ParseArrayLiteral parser =
            let ele, parser' = parser |> ParseCallExpressionList TokenType.RBRACKET
            { Token = parser.CurrentToken
              Elements = ele }:> INode, parser'
        
        let private ParseIndexExpression exp parser =
            let ind, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
            
            let ind', parser'' = match parser' |> ExpectPeek TokenType.RBRACKET with
                                    | true, p' -> ind, p'
                                    | false, p' -> Unchecked.defaultof<Expression>, p'
                
            { Token = parser.CurrentToken
              Left = exp
              Index = ind' }:> INode, parser''
            
        let rec private ParseHashLiteralRec acc exp parser =
            let acc' = match exp with
                            | Some e -> acc @ [ e ]
                            | _ -> acc
            
            match parser |> PeekTokenIs TokenType.RBRACE with
                | true -> match parser |> ExpectPeek TokenType.RBRACE with
                            | true, p' -> acc', p'
                            | false, p' -> Unchecked.defaultof<(Expression * Expression) list>, p'
                | false -> let exp, parser' = parser |> NextToken |> ParseExpression PrecedenceType.LOWEST
                           match parser' |> ExpectPeek TokenType.COLON with
                                | true, p'' -> let exp', parser'' = parser' |> NextToken |> ParseExpression PrecedenceType.LOWEST
                                               match parser'' |> PeekTokenIs TokenType.RBRACE with
                                                    | true -> match parser'' |> ExpectPeek TokenType.COMMA with
                                                                | true, p''' -> ParseHashLiteralRec acc' (Some (exp, exp')) p'''
                                                                | false, p''' -> Unchecked.defaultof<(Expression * Expression) list>, p'''
                                                    | false -> Unchecked.defaultof<(Expression * Expression) list>, p''   
                                | false, p'' -> Unchecked.defaultof<(Expression * Expression) list>, p''              
        
        let private ParseHashLiteral parser =
            let pairs, parser' = ParseHashLiteralRec [] None parser
            { Token = parser.CurrentToken
              Pair = pairs |> Array.ofList }:> INode, parser'
                 
        let NewParser (lexer: Lexer): Parser =
            { Lexer = lexer
              Error = list.Empty
              CurrentToken = Unchecked.defaultof<Token>
              PeekToken = Unchecked.defaultof<Token>
              PrefixParseFns = Map.empty
              InfixParseFns = Map.empty }
            |> RegisterPrefix TokenType.IDENT (PrefixParseFn ParseIdentifier)
            |> RegisterPrefix TokenType.INT (PrefixParseFn ParseIntegerLiteral)
            |> RegisterPrefix TokenType.STRING (PrefixParseFn ParseStringLiteral)
            |> RegisterPrefix TokenType.BANG (PrefixParseFn ParsePrefixExpression)
            |> RegisterPrefix TokenType.MINUS (PrefixParseFn ParsePrefixExpression)
            |> RegisterPrefix TokenType.TRUE (PrefixParseFn ParseBoolean)
            |> RegisterPrefix TokenType.FALSE (PrefixParseFn ParseBoolean)
            |> RegisterPrefix TokenType.LPAREN (PrefixParseFn ParseGroupedExpression)
            |> RegisterPrefix TokenType.IF (PrefixParseFn ParseIfExpression)
            |> RegisterPrefix TokenType.FUNCTION (PrefixParseFn ParseFunctionLiteral)
            |> RegisterPrefix TokenType.LBRACKET (PrefixParseFn ParseArrayLiteral)
            |> RegisterPrefix TokenType.LBRACE (PrefixParseFn ParseHashLiteral)
            |> RegisterInfix TokenType.PLUS (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.MINUS (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.SLASH (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.ASTERISK (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.EQ (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.NOT_EQ (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.LT (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.GT (InfixParseFn ParseInfixExpression)
            |> RegisterInfix TokenType.LPAREN (InfixParseFn ParseCallExpression)
            |> RegisterInfix TokenType.LBRACKET (InfixParseFn ParseIndexExpression)
            |> NextToken
            |> NextToken
        
        let rec private ParseProgramRec accumulator stmt parser =
            let accumulator' = match stmt with
                                    | Some a -> accumulator @ [ a ]
                                    | _ -> accumulator
            match parser |> CurrentTokenIs TokenType.EOF with
                | true -> accumulator', parser
                | _ -> let nextStatement, parser' = parser |> ParseStatement
                       ParseProgramRec accumulator' (Some nextStatement) parser'
                               
        let ParseProgram parser =
                let stmts, parser' = parser |> ParseProgramRec [] None 
                { Statements = stmts |> Array.ofList }, parser'
