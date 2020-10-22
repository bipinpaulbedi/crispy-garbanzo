namespace MonkeyInterpreter

open System

module Lexer =
    open System
    open Token
        type Lexer =
            { Input: string
              Position: int
              ReadPosition: int
              Ch: Option<char> }

        let private PeekChar l =
            match l with
            | { ReadPosition = pos } when pos >= l.Input.Length ->
                Option.None
            | _ ->
                Some l.Input.[l.ReadPosition]
                
        let private ReadChar l =
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

        let NewLexer input =
            { Input = input
              Position = 0
              ReadPosition = 0
              Ch = Option.None }
            |> ReadChar
            
        let rec private SkipWhitespace currentLexer =
            match currentLexer.Ch with
            | Some ' ' -> ReadChar(currentLexer) |> SkipWhitespace
            | Some '\t' -> ReadChar(currentLexer) |> SkipWhitespace
            | Some '\n' -> ReadChar(currentLexer) |> SkipWhitespace
            | Some '\r' -> ReadChar(currentLexer) |> SkipWhitespace
            | _ -> currentLexer
        
        let rec private ReadNumber accumulator currentLexer previousLexor =
            match currentLexer.Ch with
            | Some x ->
                match x with
                | x when Char.IsNumber(x) -> ReadNumber (accumulator.ToString() + x.ToString()) (ReadChar(currentLexer)) currentLexer
                | _ -> (accumulator, previousLexor)
            | _ -> (accumulator, previousLexor)
            
        let rec private ReadIdentifier accumulator currentLexer previousLexor =
            match currentLexer.Ch with
            | Some x ->
                match x with
                | x when Char.IsLetter(x) -> ReadIdentifier (accumulator.ToString() + x.ToString()) (ReadChar(currentLexer)) currentLexer
                | '_' -> ReadIdentifier (accumulator.ToString() + "_") (ReadChar(currentLexer)) currentLexer
                | _ -> (accumulator, previousLexor)
            | _ -> (accumulator, previousLexor)
              
        let private NextTokenEval currentLexer =
            match currentLexer.Ch with
            | Some '=' when (PeekChar currentLexer) = Some '=' ->
                ({ Type = TokenType.EQ
                   Literal = Some "==" }, ReadChar(currentLexer))
            | Some '=' ->
                ({ Type = TokenType.ASSIGN
                   Literal = Some "=" }, currentLexer)
            | Some '+' ->
                ({ Type = TokenType.PLUS
                   Literal = Some "+" }, currentLexer)
            | Some '-' ->
                ({ Type = TokenType.MINUS
                   Literal = Some "-" }, currentLexer)
            | Some '!' when (PeekChar currentLexer) = Some '=' ->
                ({ Type = TokenType.NOTEQ
                   Literal = Some "!=" }, ReadChar(currentLexer))
            | Some '!' ->
                ({ Type = TokenType.BANG
                   Literal = Some "!" }, currentLexer)
            | Some '/' ->
                ({ Type = TokenType.SLASH
                   Literal = Some "/" }, currentLexer)
            | Some '*' ->
                ({ Type = TokenType.ASTERISK
                   Literal = Some "*" }, currentLexer)
            | Some '<' ->
                ({ Type = TokenType.LT
                   Literal = Some "<" }, currentLexer)
            | Some '>' ->
                ({ Type = TokenType.GT
                   Literal = Some ">" }, currentLexer) 
            | Some ';' ->
                ({ Type = TokenType.SEMICOLON
                   Literal = Some ";" }, currentLexer)
            | Some ',' ->
                ({ Type = TokenType.COMMA
                   Literal = Some "," }, currentLexer)
            | Some '(' ->
                ({ Type = TokenType.LPAREN
                   Literal = Some "(" }, currentLexer)
            | Some ')' ->
                ({ Type = TokenType.RPAREN
                   Literal = Some ")" }, currentLexer)
            | Some '{' ->
                ({ Type = TokenType.LBRACE
                   Literal = Some "{" }, currentLexer)
            | Some '}' ->
                ({ Type = TokenType.RBRACE
                   Literal =  Some "}" }, currentLexer)
            | Some currentChar when Char.IsLetter(currentChar) ->
                let (literal, activeLexer) = ReadIdentifier "" currentLexer currentLexer
                ({ Type = LookupIdentifier(literal)
                   Literal = Some literal }, activeLexer)
            | Some currentChar when Char.IsNumber(currentChar) ->
                let (literal, activeLexer) = ReadNumber "" currentLexer currentLexer
                ({ Type = TokenType.INT
                   Literal = Some literal }, activeLexer )
            | None ->
                ({ Type = TokenType.EOF
                   Literal = None }, currentLexer)
            | _ ->
                ({ Type = TokenType.ILLEGAL
                   Literal = None }, currentLexer)
                 
        let NextToken lexer =
           let (t, activeLexer) = NextTokenEval (SkipWhitespace lexer)
           (t , activeLexer |> ReadChar)
           
        let rec ProcessLexerToTokens accumulator (token, lexer) =
            let accumulator' = accumulator @ [ token ]
            match token with
            | { Type = TokenType.EOF; Literal = None } -> accumulator'
            | _ -> ProcessLexerToTokens accumulator' (NextToken lexer) 
