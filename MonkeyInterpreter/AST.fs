namespace MonkeyInterpreter

module AST =
    
    open System
    open System.Collections.Generic
    open Token
    let private TokenLiteral' token =
        match token.Literal with
        | Some literal -> literal
        | None -> ""
    
    type INode =
        abstract member TokenLiteral : unit -> string
    
    type Statement = INode
    type Expression = INode
    
    type Program =
         { Statements : Statement[] }
         
         interface INode with
            member this.TokenLiteral() =
                match this.Statements.Length with
                | len when len > 0 -> this.Statements.[0].TokenLiteral()
                | _ -> ""
         override this.ToString() =
            this.Statements
            |> Array.map (fun t -> t.ToString())
            |> String.concat ""
    
    type Identifier =
        { Token: Token
          Value: string }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = this.Value
            
    type LetStatement =
         { Token : Token
           Name : Identifier
           Value: Option<Expression> }
         
         interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
         override this.ToString() = String.Format("{0} {1} = {2};", (this :> INode).TokenLiteral(),
                                                 this.Name, this.Value |> Option.defaultValue Unchecked.defaultof<Expression>)
                
    type ReturnStatement =
         { Token : Token
           ReturnValue: Option<Expression> }
        
         interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
         override this.ToString() = String.Format("{0} {1};", (this :> INode).TokenLiteral(), this.ReturnValue |> Option.defaultValue Unchecked.defaultof<Expression>)
    
    type ExpressionStatement =
         { Token : Token
           Expression: Option<Expression> }
         
         interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'   
         override this.ToString() = String.Format("{0}", this.Expression |> Option.defaultValue Unchecked.defaultof<Expression>)
    
    type BlockStatement =
         { Token : Token
           Statements: Statement[] }
         
         interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
         override this.ToString() =
            this.Statements
            |> Array.map (fun this -> this.ToString())
            |> String.concat ""
    
    type Boolean =
        { Token : Token
          BoolValue: bool }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = (this :> INode).TokenLiteral()
    
    type IntegerLiteral =
        { Token : Token
          IntValue: int64 }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = (this :> INode).TokenLiteral()    
   
    type PrefixExpression =
        { Token : Token
          Operator: string
          Right: Expression }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = String.Format("({0}{1})", this.Operator, this.Right)
    
    type InfixExpression =
        { Token : Token
          Left: Expression
          Operator: string
          Right: Expression }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = String.Format("({0} {1} {2})", this.Left, this.Operator, this.Right)
    
    type IfExpression =
        { Token : Token
          Condition: INode
          Consequence: BlockStatement
          Alternative: Option<BlockStatement> }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = String.Format("if{0} {1}else {2}", this.Condition, this.Consequence, this.Alternative |> Option.defaultValue Unchecked.defaultof<BlockStatement>)
    
    type FunctionLiteral =
        { Token : Token
          Parameters: Identifier[]
          Body: BlockStatement}
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() =
            let parameters =
                this.Parameters
                |> Array.map (fun p -> p.ToString())
                |> String.concat ", "
            
            String.Format("{0}({1}) {2}", (this :> INode).TokenLiteral(), parameters, this.Body)
    
    type CallExpression =
        { Token : Token
          Function : Expression
          Arguments : Expression[] }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() =
            let args =
                this.Arguments
                |> Array.map (fun a -> a.ToString())
                |> String.concat ", "
            
            String.Format("{0}({1})", this.Function, args)
    
    type StringLiteral =
        { Token : Token
          StringValue: string }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = (this :> INode).TokenLiteral()
    
    type ArrayLiteral =
        { Token : Token
          Elements : Expression[] }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() =
            let ele =
                this.Elements
                |> Array.map (fun el -> el.ToString())
                |> String.concat ", "
            
            String.Format("[{0}]", ele)
    
    type IndexExpression =
        { Token : Token
          Left : Expression
          Index : Expression }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() = String.Format("({0}[{1}])", this.Left, this.Index)
    
    type HashLiteral =
        { Token : Token
          Pair : (Expression * Expression)[] }
        
        interface INode with
            member this.TokenLiteral() = this.Token |> TokenLiteral'
        override this.ToString() =
            let pr =
                this.Pair
                |> Array.map (fun (key, value) -> String.Format("{0}:{1}", key.ToString(), value.ToString()))
                |> String.concat ", "
            
            String.Format("{{{0}}}", pr)
