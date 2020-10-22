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

        let NewLexer inp =
            { Input = inp
              Position = 0
              ReadPosition = 0
              Ch = Option.None }
            |> ReadChar
            
        let rec private SkipWhitespace l =
            match l.Ch with
            | Some ' ' -> ReadChar(l) |> SkipWhitespace
            | Some '\t' -> ReadChar(l) |> SkipWhitespace
            | Some '\n' -> ReadChar(l) |> SkipWhitespace
            | Some '\r' -> ReadChar(l) |> SkipWhitespace
            | _ -> l
        
        let rec private ReadNumber acc l p =
            match l.Ch with
            | Some x ->
                match x with
                | x when Char.IsNumber(x) -> ReadNumber (acc.ToString() + x.ToString()) (ReadChar(l)) l
                | _ -> (acc, p)
            | _ -> (acc, p)
            
        let rec private ReadIdentifier acc l p =
            match l.Ch with
            | Some x ->
                match x with
                | x when Char.IsLetter(x) -> ReadIdentifier (acc.ToString() + x.ToString()) (ReadChar(l)) l
                | '_' -> ReadIdentifier (acc.ToString() + "_") (ReadChar(l)) l
                | _ -> (acc, p)
            | _ -> (acc, p)
              
        let private NextTokenEval l =
            match l.Ch with
            | Some '=' when (PeekChar l) = Some '=' ->
                ({ Type = TokenType.EQ
                   Literal = Some "==" }, ReadChar(l))
            | Some '=' ->
                ({ Type = TokenType.ASSIGN
                   Literal = Some "=" }, l)
            | Some '+' ->
                ({ Type = TokenType.PLUS
                   Literal = Some "+" }, l)
            | Some '-' ->
                ({ Type = TokenType.MINUS
                   Literal = Some "-" }, l)
            | Some '!' when (PeekChar l) = Some '=' ->
                ({ Type = TokenType.NOTEQ
                   Literal = Some "!=" }, ReadChar(l))
            | Some '!' ->
                ({ Type = TokenType.BANG
                   Literal = Some "!" }, l)
            | Some '/' ->
                ({ Type = TokenType.SLASH
                   Literal = Some "/" }, l)
            | Some '*' ->
                ({ Type = TokenType.ASTERISK
                   Literal = Some "*" }, l)
            | Some '<' ->
                ({ Type = TokenType.LT
                   Literal = Some "<" }, l)
            | Some '>' ->
                ({ Type = TokenType.GT
                   Literal = Some ">" }, l) 
            | Some ';' ->
                ({ Type = TokenType.SEMICOLON
                   Literal = Some ";" }, l)
            | Some ',' ->
                ({ Type = TokenType.COMMA
                   Literal = Some "," }, l)
            | Some '(' ->
                ({ Type = TokenType.LPAREN
                   Literal = Some "(" }, l)
            | Some ')' ->
                ({ Type = TokenType.RPAREN
                   Literal = Some ")" }, l)
            | Some '{' ->
                ({ Type = TokenType.LBRACE
                   Literal = Some "{" }, l)
            | Some '}' ->
                ({ Type = TokenType.RBRACE
                   Literal =  Some "}" }, l)
            | Some x when Char.IsLetter(x) ->
                let (literal, lx) = ReadIdentifier "" l l
                ({ Type = LookupIdentifier(literal)
                   Literal = Some literal }, lx)
            | Some x when Char.IsNumber(x) ->
                let (literal, lx) = ReadNumber "" l l
                ({ Type = TokenType.INT
                   Literal = Some literal }, lx)
            | None ->
                ({ Type = TokenType.EOF
                   Literal = None }, l)
            | _ ->
                ({ Type = TokenType.ILLEGAL
                   Literal = None }, l)
                 
        let NextToken(l: Lexer) =
           let (t, l) = NextTokenEval (SkipWhitespace l)
           (t , l |> ReadChar)
