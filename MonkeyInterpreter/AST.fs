namespace MonkeyInterpreter

open System

module AST =
    
    open Token
    let TokenLiteral token =
        match token.Literal with
        | Some lit -> lit
        | None -> ""
    
    type INode =
        abstract member LiteralFromToken : unit -> string
    
    type Program =
         { Statements : INode[] }
         
         interface INode with
            member this.LiteralFromToken() = this.Statements.[0].LiteralFromToken()
         override this.ToString() =
            this.Statements
            |> Array.map (fun this -> this.ToString())
            |> String.concat ""
    
    type Identifier =
        { Token: Token
          Value: string }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() = this.Value
            
    type LetStatement =
         { Token : Token
           Name : Identifier
           Value: Option<INode> }
         
         interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
         override this.ToString() =
                match this.Value with
                | Some value -> String.Format("{0} {1} = {2};", (this :> INode).LiteralFromToken(),
                                                 this.Name, value)
                | None -> String.Format("{0} {1} = ;", (this :> INode).LiteralFromToken(),
                                                 this.Name)
                
    type ReturnStatement =
         { Token : Token
           ReturnValue: Option<INode> }
        
         interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral   
         override this.ToString() =
                match this.ReturnValue with
                | Some returnValue -> String.Format("{0} {1};", (this :> INode).LiteralFromToken(), returnValue)
                | None -> String.Format("{0} ;", (this :> INode).LiteralFromToken())
    
    type ExpressionStatement =
         { Token : Token
           Expression: Option<INode> }
         
         interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral   
         override this.ToString() =
                match this.Expression with
                | Some expression -> String.Format("{0} {1};", (this :> INode).LiteralFromToken(), expression)
                | None -> String.Format("{0} ;", (this :> INode).LiteralFromToken())
    
    type BlockStatement =
         { Token : Token
           Statements: Option<INode>[] }
         
         interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
         override this.ToString() =
            this.Statements
            |> Array.map (fun this -> this |> Option.defaultValue Unchecked.defaultof<INode>)
            |> Array.map (fun this -> this.ToString())
            |> String.concat ""
    
    type Boolean =
        { Token : Token
          BoolValue: bool }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() = (this :> INode).LiteralFromToken()
    
    type IntegerLiteral =
        { Token : Token
          IntValue: int64 }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() = (this :> INode).LiteralFromToken()    
   
    type PrefixExpression =
        { Token : Token
          Operator: string
          Right: INode }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() = String.Format("({0}{1})", this.Operator, this.Right)
    
    type InfixExpression =
        { Token : Token
          Left: INode
          Operator: string
          Right: INode }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() = String.Format("({0} {1} {2})", this.Left, this.Operator, this.Right)
    
    type IfExpression =
        { Token : Token
          Condition: INode
          Consequence: BlockStatement
          Alternative: Option<BlockStatement> }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() =
            match this.Alternative with
            | Some alternative -> String.Format("if {0} {1} {2}", this.Condition, this.Consequence, alternative)
            | None -> String.Format("if {0} {1}", this.Condition, this.Consequence)
    
    type FunctionLiteral =
        { Token : Token
          Parameters: Identifier[]
          Body: BlockStatement}
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() =
            let prms =
                this.Parameters
                |> Array.map (fun param -> param.ToString())
                |> String.concat ", "
            
            String.Format("{0}({1}){2}", (this :> INode).LiteralFromToken(), prms, this.Body)
    
    type CallExpression =
        { Token : Token
          Function: INode
          Arguments: INode[] }
        
        interface INode with
            member this.LiteralFromToken() = this.Token |> TokenLiteral
        override this.ToString() =
            let args =
                this.Arguments
                |> Array.map (fun param -> param.ToString())
                |> String.concat ", "
            
            String.Format("{0}({1})", this.Function, args)