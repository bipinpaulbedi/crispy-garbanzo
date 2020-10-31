namespace MonkeyInterpreter

open System
open MonkeyInterpreter.AST

module ParserTests =
    
    open FsUnit
    open Xunit
    open Lexer
    open Parser
    open AST
    
    let TestLetStatement exp (inp:INode) name =
        inp.LiteralFromToken() |> should equal "let"
        (inp :?> LetStatement).Name.Value |> should equal exp
        ((inp :?> LetStatement).Name :> INode).LiteralFromToken() |> should equal exp
    
    let TestIntegerLiteral (exp:int64) (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString())
        (inp :?> IntegerLiteral).IntValue |> should equal exp
        
    let TestIdentifier exp (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString())
        (inp :?> Identifier).Value |> should equal exp
    
    let TestBooleanLiteral exp (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString().ToLower())
        (inp :?> Boolean).BoolValue |> should equal exp
    
    let TestLiteralExpression (exp:Object) (inp:Option<INode>) =
        match inp with
            | Some input -> match exp with
                                | :? int as i -> input |> TestIntegerLiteral (int64(i))
                                | :? int64 as i -> input |> TestIntegerLiteral i
                                | :? string as s -> input |> TestIdentifier s
                                | :? bool as b -> input |> TestBooleanLiteral b
                                | _ -> 1 |> should equal 1
            | _ -> 1 |> should equal 1
    
    let TestExpressionStatement exp (inp:INode) =
        inp.LiteralFromToken() |> should equal (exp.ToString())
        (Some (inp :?> ExpressionStatement).Expression.Value) |> TestLiteralExpression exp |> ignore
    
    let TestPrefixExpressionStatement (operator,right) (inp:INode) =
        inp.ToString() |> should equal (String.Format("({0}{1})", operator, right.ToString().ToLower()))
        (inp :?> PrefixExpression).Operator |> should equal operator
        (Some (inp :?> PrefixExpression).Right) |> TestLiteralExpression right
        
    let TestInfixExpressionStatement (left, operator,right) (inp:INode) =
        inp.ToString() |> should equal (String.Format("({0}{1}{2})",left.ToString().ToLower(), operator, right.ToString().ToLower()))
        (inp :?> InfixExpression).Operator |> should equal operator
        (Some (inp :?> InfixExpression).Right) |> TestLiteralExpression right
        (Some (inp :?> InfixExpression).Left) |> TestLiteralExpression left
        
            
    [<Theory>]
    [<InlineData("let x = 5;", "x", 5)>]
    [<InlineData("let y = true;", "y", true)>]
    [<InlineData("let foobar = y;", "foobar", "y")>]  
    let ``Test Let Statement`` inp expIdentifier expValue =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should greaterThan 0
        prg.Statements.[0] |> TestLetStatement expIdentifier |> ignore
        (prg.Statements.[0] :?> LetStatement).Value |> TestLiteralExpression expValue |> ignore
    
    [<Theory>]
    [<InlineData("return 5;", 5)>]
    [<InlineData("return true;", true)>]
    [<InlineData("return foobar;", "foobar")>]  
    let ``Test Return Statement`` inp expValue =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should greaterThan 0
        prg.Statements.[0].LiteralFromToken() |> should equal "return"
        (prg.Statements.[0] :?> ReturnStatement).ReturnValue |> TestLiteralExpression expValue |> ignore
        
    [<Theory>]
    [<InlineData("foobar;")>]
    let ``Test Identifier Expression`` inp =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should equal 1
        (prg.Statements.[0] :?> ExpressionStatement) |> TestExpressionStatement "foobar" |> ignore

    [<Theory>]
    [<InlineData("5;")>]
    let ``Test Integer Literal Expression`` inp =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should equal 1
        (prg.Statements.[0] :?> ExpressionStatement) |> TestExpressionStatement 5 |> ignore
    
    [<Theory>]
    [<InlineData("!5;", "!", 5)>]
    [<InlineData("-15;", "-", 15)>]
    [<InlineData("!foobar;", "!", "foobar")>]
    [<InlineData("-foobar;", "-", "foobar")>]
    [<InlineData("!true;", "!", true)>]
    [<InlineData("!false;", "!", false)>]
    let ``Test Parsing Prefix Expression`` inp operator right =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should equal 1
        (prg.Statements.[0] :?> ExpressionStatement).Expression.Value |> TestPrefixExpressionStatement (operator, right) |> ignore
    
    [<Theory>]
    [<InlineData("5 + 5;", 5, "+", 5)>]
    [<InlineData("5 - 5;", 5, "-", 5)>]
    [<InlineData("5 * 5;", 5, "*", 5)>]
    [<InlineData("5 / 5;", 5, "/", 5)>]
    [<InlineData("5 > 5;", 5, ">", 5)>]
    [<InlineData("5 < 5;", 5, "<", 5)>]
    [<InlineData("5 == 5;", 5, "==", 5)>]
    [<InlineData("5 != 5;", 5, "!=", 5)>]
    [<InlineData("foobar + barfoo;", "foobar", "+", "barfoo")>]
    [<InlineData("foobar - barfoo;", "foobar", "-", "barfoo")>]
    [<InlineData("foobar * barfoo;", "foobar", "*", "barfoo")>]
    [<InlineData("foobar / barfoo;", "foobar", "/", "barfoo")>]
    [<InlineData("foobar > barfoo;", "foobar", ">", "barfoo")>]
    [<InlineData("foobar < barfoo;", "foobar", "<", "barfoo")>]
    [<InlineData("foobar == barfoo;", "foobar", "==", "barfoo")>]
    [<InlineData("foobar != barfoo;", "foobar", "!=", "barfoo")>]
    [<InlineData("true == true", true, "==", true)>]
    [<InlineData("true != false", true, "!=", false)>]
    [<InlineData("false == false", false, "==", false)>]
    let ``Test Parsing Infix Expression`` inp left operator right =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should equal 1
        (prg.Statements.[0] :?> ExpressionStatement).Expression.Value |> TestInfixExpressionStatement (left, operator, right) |> ignore
        
    [<Theory>]
    [<InlineData("-a * b", "((-a)*b)")>]
    [<InlineData("!-a", "(!(-a))")>]
    //[<InlineData("a + b + c", "((a+b)+c)")>]
    //[<InlineData("a + b - c", "((a+b)-c)")>]
    [<InlineData("a * b * c", "((a*b)*c)")>]
    //[<InlineData("a * b / c", "((a*b)/c)")>]
    [<InlineData("a * (b / c)", "(a*(b/c))")>]
    [<InlineData("a + b / c", "(a+(b/c))")>]
    [<InlineData("a + b * c + d / e - f", "(((a+(b*c))+(d/e))-f)")>]
    [<InlineData("3 + 4; -5 * 5", "(3+4)((-5)*5)")>]
    [<InlineData("5 > 4 == 3 < 4", "((5>4)==(3<4))")>]
    [<InlineData("5 < 4 != 3 > 4", "((5<4)!=(3>4))")>]
    [<InlineData("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3+(4*5))==((3*1)+(4*5)))")>]
    [<InlineData("true", "true")>]
    [<InlineData("false", "false")>]
    [<InlineData("3 > 5 == false", "((3>5)==false)")>]
    [<InlineData("3 < 5 == true", "((3<5)==true)")>]
    //[<InlineData("1 + (2 + 3) + 4", "((1+(2+3))+4)")>]
    [<InlineData("(5 + 5) * 2", "((5+5)*2)")>]
    [<InlineData("2 / (5 + 5)", "(2/(5+5))")>]
    //[<InlineData("(5 + 5) * 2 * (5 + 5)", "(((5+5)*2)*(5+5))")>]
    [<InlineData("-(5 + 5)", "(-(5+5))")>]
    [<InlineData("!(true == true)", "(!(true==true))")>]
    [<InlineData("a + add(b * c) + d", "((a+add((b*c)))+d)")>]
    [<InlineData("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a,b,1,(2*3),(4+5),add(6,(7*8)))")>]
    [<InlineData("add(a + b + c * d / f + g)", "add((((a+b)+((c*d)/f))+g))")>]
    let ``Test Operator Precedence Parsing`` inp exp =
        let prg, _ = NewLexer inp
                            |> NewParser
                            |> ParseProgram
        
        prg.Statements.Length |> should equal 1
        prg.ToString() |> should equal exp