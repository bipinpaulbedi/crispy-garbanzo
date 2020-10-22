namespace MonkeyInterpreter
module LexerTests =

    open FsUnit
    open Xunit
    open Token
    open Lexer
    

    [<Fact>]
    let ``Test Next Token``() =
        let input = "let five = 5;
                      let ten = 10;

                      let add = fn(x, y) {
                        x + y;
                      };

                      let result = add(five, ten);
                      !-/*5;
                      5 < 10 > 5;

                      if (5 < 10) {
	                      return true;
                      } else {
	                      return false;
                      }

                      10 == 10;
                      10 != 9;"

        let expected =
            [ { Type = TokenType.LET; Literal = Some "let"};
              { Type = TokenType.IDENT; Literal = Some "five"};
              { Type = TokenType.ASSIGN; Literal = Some "="};
              { Type = TokenType.INT; Literal = Some "5"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.LET; Literal = Some "let"};
              { Type = TokenType.IDENT; Literal = Some "ten"};
              { Type = TokenType.ASSIGN; Literal = Some "="};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.LET; Literal = Some "let"};
              { Type = TokenType.IDENT; Literal = Some "add"};
              { Type = TokenType.ASSIGN; Literal = Some "="};
              { Type = TokenType.FUNCTION; Literal = Some "fn"};
              { Type = TokenType.LPAREN; Literal = Some "("};
              { Type = TokenType.IDENT; Literal = Some "x"};
              { Type = TokenType.COMMA; Literal = Some ","};
              { Type = TokenType.IDENT; Literal = Some "y"};
              { Type = TokenType.RPAREN; Literal = Some ")"};
              { Type = TokenType.LBRACE; Literal = Some "{"};
              { Type = TokenType.IDENT; Literal = Some "x"};
              { Type = TokenType.PLUS; Literal = Some "+"};
              { Type = TokenType.IDENT; Literal = Some "y"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.RBRACE; Literal = Some "}"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.LET; Literal = Some "let"};
              { Type = TokenType.IDENT; Literal = Some "result"};
              { Type = TokenType.ASSIGN; Literal = Some "="};
              { Type = TokenType.IDENT; Literal = Some "add"};
              { Type = TokenType.LPAREN; Literal = Some "("};
              { Type = TokenType.IDENT; Literal = Some "five"};
              { Type = TokenType.COMMA; Literal = Some ","};
              { Type = TokenType.IDENT; Literal = Some "ten"};
              { Type = TokenType.RPAREN; Literal = Some ")"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.BANG; Literal = Some "!"};
              { Type = TokenType.MINUS; Literal = Some "-"};
              { Type = TokenType.SLASH; Literal = Some "/"};
              { Type = TokenType.ASTERISK; Literal = Some "*"};
              { Type = TokenType.INT; Literal = Some "5"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.INT; Literal = Some "5"};
              { Type = TokenType.LT; Literal = Some "<"};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.GT; Literal = Some ">"};
              { Type = TokenType.INT; Literal = Some "5"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.IF; Literal = Some "if"};
              { Type = TokenType.LPAREN; Literal = Some "("};
              { Type = TokenType.INT; Literal = Some "5"};
              { Type = TokenType.LT; Literal = Some "<"};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.RPAREN; Literal = Some ")"};
              { Type = TokenType.LBRACE; Literal = Some "{"};
              { Type = TokenType.RETURN; Literal = Some "return"};
              { Type = TokenType.TRUE; Literal = Some "true"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.RBRACE; Literal = Some "}"};
              { Type = TokenType.ELSE; Literal = Some "else"};
              { Type = TokenType.LBRACE; Literal = Some "{"};
              { Type = TokenType.RETURN; Literal = Some "return"};
              { Type = TokenType.FALSE; Literal = Some "false"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.RBRACE; Literal = Some "}"};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.EQ; Literal = Some "=="};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.INT; Literal = Some "10"};
              { Type = TokenType.NOTEQ; Literal = Some "!="};
              { Type = TokenType.INT; Literal = Some "9"};
              { Type = TokenType.SEMICOLON; Literal = Some ";"};
              { Type = TokenType.EOF; Literal = None} ]

        let l = NewLexer(input)

        let rec ProcessLexer (token, lexer) accumulator =
            let accumulator' = accumulator @ [ token ]
            match token with
            | { Type = TokenType.EOF; Literal = None } -> accumulator'
            | _ -> ProcessLexer (NextToken lexer) accumulator'

        let result = ProcessLexer (NextToken l) []

        result |> should equal expected
