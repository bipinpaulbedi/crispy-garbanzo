namespace MonkeyInterpreter
    module Lexer =
        open System
        open Token
            type Lexer =
                { Input: string
                  Position: int
                  ReadPosition: int
                  Ch: Option<char> }
                
            let private ReadChar lexer =
                match lexer with
                | { ReadPosition = pos } when pos >= lexer.Input.Length ->
                    { Input = lexer.Input
                      Position = lexer.ReadPosition
                      ReadPosition = lexer.ReadPosition + 1
                      Ch = Option.None }
                | _ ->
                    { Input = lexer.Input
                      Position = lexer.ReadPosition
                      ReadPosition = lexer.ReadPosition + 1
                      Ch = Some lexer.Input.[lexer.ReadPosition] }

            let NewLexer input =
                { Input = input
                  Position = 0
                  ReadPosition = 0
                  Ch = Option.None }
                |> ReadChar
                
            let rec private SkipWhitespaceRec lexer =
                match lexer.Ch with
                | Some ' ' -> ReadChar(lexer) |> SkipWhitespaceRec
                | Some '\t' -> ReadChar(lexer) |> SkipWhitespaceRec
                | Some '\n' -> ReadChar(lexer) |> SkipWhitespaceRec
                | Some '\r' -> ReadChar(lexer) |> SkipWhitespaceRec
                | _ -> lexer
            
            let rec private ReadNumberRec accumulator lexer' lexer =
                match lexer'.Ch with
                | Some num ->
                    match num with
                    | num when Char.IsNumber(num) -> ReadNumberRec (accumulator.ToString() + num.ToString()) (ReadChar(lexer')) lexer'
                    | _ -> (accumulator, lexer)
                | _ -> (accumulator, lexer)
            
            let rec private ReadStringRec accumulator lexer' lexer =
                match lexer'.Ch with
                | Some ch ->
                    match ch with
                    | '"' -> (accumulator, lexer)
                    | ch -> ReadStringRec (accumulator.ToString() + ch.ToString()) (ReadChar(lexer')) lexer'
                | _ -> (accumulator, lexer)
                
            let rec private ReadIdentifierRec accumulator lexer' lexer =
                match lexer'.Ch with
                | Some ch ->
                    match ch with
                    | ch when Char.IsLetter(ch) -> ReadIdentifierRec (accumulator.ToString() + ch.ToString()) (ReadChar(lexer')) lexer'
                    | '_' -> ReadIdentifierRec (accumulator.ToString() + "_") (ReadChar(lexer')) lexer'
                    | _ -> (accumulator, lexer)
                | _ -> (accumulator, lexer)
                  
            let private NextToken' lexer =
                match lexer.Ch with
                | Some '=' when (ReadChar lexer).Ch = Some '=' ->
                    ({ Type = TokenType.EQ
                       Literal = Some "==" }, ReadChar(lexer))
                | Some '=' ->
                    ({ Type = TokenType.ASSIGN
                       Literal = Some "=" }, lexer)
                | Some '+' ->
                    ({ Type = TokenType.PLUS
                       Literal = Some "+" }, lexer)
                | Some '-' ->
                    ({ Type = TokenType.MINUS
                       Literal = Some "-" }, lexer)
                | Some '!' when (ReadChar lexer).Ch = Some '=' ->
                    ({ Type = TokenType.NOT_EQ
                       Literal = Some "!=" }, ReadChar(lexer))
                | Some '!' ->
                    ({ Type = TokenType.BANG
                       Literal = Some "!" }, lexer)
                | Some '/' ->
                    ({ Type = TokenType.SLASH
                       Literal = Some "/" }, lexer)
                | Some '*' ->
                    ({ Type = TokenType.ASTERISK
                       Literal = Some "*" }, lexer)
                | Some '<' ->
                    ({ Type = TokenType.LT
                       Literal = Some "<" }, lexer)
                | Some '>' ->
                    ({ Type = TokenType.GT
                       Literal = Some ">" }, lexer) 
                | Some ';' ->
                    ({ Type = TokenType.SEMICOLON
                       Literal = Some ";" }, lexer)
                | Some ',' ->
                    ({ Type = TokenType.COMMA
                       Literal = Some "," }, lexer)
                | Some '(' ->
                    ({ Type = TokenType.LPAREN
                       Literal = Some "(" }, lexer)
                | Some ')' ->
                    ({ Type = TokenType.RPAREN
                       Literal = Some ")" }, lexer)
                | Some '"' ->
                    let (literal, lexer') = ReadStringRec "" (ReadChar(lexer)) lexer
                    ({ Type = TokenType.STRING
                       Literal = Some literal }, ReadChar(lexer'))
                | Some '{' ->
                    ({ Type = TokenType.LBRACE
                       Literal = Some "{" }, lexer)
                | Some '}' ->
                    ({ Type = TokenType.RBRACE
                       Literal =  Some "}" }, lexer)
                | Some currentChar when Char.IsLetter(currentChar) ->
                    let (literal, lexer') = ReadIdentifierRec "" lexer lexer
                    ({ Type = LookupIdentifier(literal)
                       Literal = Some literal }, lexer')
                | Some currentChar when Char.IsNumber(currentChar) ->
                    let (literal, lexer') = ReadNumberRec "" lexer lexer
                    ({ Type = TokenType.INT
                       Literal = Some literal }, lexer' )
                | None ->
                    ({ Type = TokenType.EOF
                       Literal = None }, lexer)
                | _ ->
                    ({ Type = TokenType.ILLEGAL
                       Literal = None }, lexer)
                     
            let NextToken lexer =
               let (token, lexer') = NextToken' (SkipWhitespaceRec lexer)
               (token , lexer' |> ReadChar)
               
            let rec ConvertLexerToTokensRec accumulator (token, lexer) =
                let accumulator' = match token with
                                    | Some t -> accumulator @ [ t ]
                                    | _ -> accumulator
                match token with
                | Some { Type = TokenType.EOF; Literal = None } -> accumulator'
                | _ -> let (token', lexer') = lexer |> NextToken
                       ConvertLexerToTokensRec accumulator' (Some token', lexer')
