namespace MonkeyInterpreter

    module Evaluator =
        
        open MonkeyObject
        open MonkeyEnvironment
        open AST
        open BuiltIns
        
        type private EvalType =
            | PROGRAM
            | BLOCK_STATEMENT
            | PREFIX_EXPRESSION
            | INFIX_EXPRESSION
            | IF_EXPRESSION
            | IDENTIFIER
            | EXPRESSION
            | APPLY_FUNCTION
            | ARRAY_INDEX_EXPRESSION
            | HASH_INDEX_EXPRESSION
            | HASH_LITERAL
        
        type private EvalParameters =
            | Program of AST.Program
            | BlockStatement of AST.BlockStatement
            | PrefixExpression of string * IObject
            | InfixExpression of string * IObject * IObject
            | IfExpression of AST.IfExpression
            | Identifier of AST.Identifier
            | ExpressionCollection of Expression[]
            | ApplyFunction of IObject * IObject list
            | ArrayIndexExpression of IObject * IObject
            | HashIndexExpression of IObject * IHashableObject
            | HashLiteral of HashLiteral
        
        type private EvalFn = delegate of MonkeyEnvironment * Map<EvalType, EvalFn> * EvalParameters -> IObject list
        let private MonkeyNull = Unchecked.defaultof<NULL> :> IObject
        let private MonkeyTrue = {MonkeyObject.Boolean.Value = true} :> IObject
        let private MonkeyFalse = {MonkeyObject.Boolean.Value = false} :> IObject
        
        let private BoolToMonkeyBool value =
            match value with
                | true -> MonkeyTrue
                | false -> MonkeyFalse
        
        let private InvokeEval env (map:Map<EvalType, EvalFn>) typ (node:EvalParameters) =
            match map.TryFind(typ) with
                | Some fn -> fn.Invoke (env, map, node)
                | None -> [] @ [ MonkeyNull ]
        
        let rec private EvalRec env map (node:INode) =
            match node with
                | :? Program as p -> InvokeEval env map EvalType.PROGRAM (EvalParameters.Program p)
                | :? BlockStatement as bs -> InvokeEval env map EvalType.BLOCK_STATEMENT (EvalParameters.BlockStatement bs)
                | :? ExpressionStatement as es -> EvalRec env map (es.Expression |> Option.defaultValue Unchecked.defaultof<INode>)
                | :? ReturnStatement as rs -> let v = EvalRec env map (rs.ReturnValue |> Option.defaultValue Unchecked.defaultof<INode>) 
                                              [] @ [ { ReturnValue.Value = v.[0] } :> IObject ]
                | :? LetStatement as ls -> let v = EvalRec env map (ls.Value |> Option.defaultValue Unchecked.defaultof<INode>)
                                           SetEnv ls.Name.Value v.[0] env |> ignore
                                           [] @ [ MonkeyNull ]
                | :? IntegerLiteral as i -> [] @ [ { Integer.Value = i.IntValue } :> IObject ]
                | :? StringLiteral as sl -> [] @ [ { String.Value = sl.StringValue } :> IObject ]
                | :? Boolean as b -> [] @ [ { Boolean.Value = b.BoolValue } :> IObject ]
                | :? PrefixExpression as pe -> InvokeEval env map EvalType.PREFIX_EXPRESSION (EvalParameters.PrefixExpression (pe.Operator,(EvalRec env map pe.Right).[0]))
                | :? InfixExpression as ie -> InvokeEval env map EvalType.INFIX_EXPRESSION (EvalParameters.InfixExpression (ie.Operator,(EvalRec env map ie.Left).[0],(EvalRec env map ie.Right).[0]))
                | :? IfExpression as ife -> InvokeEval env map EvalType.IF_EXPRESSION (EvalParameters.IfExpression ife)
                | :? Identifier as i -> InvokeEval env map EvalType.IDENTIFIER (EvalParameters.Identifier i)
                | :? FunctionLiteral as fl -> [] @ [  { Parameters = fl.Parameters
                                                        Body = fl.Body
                                                        Env = env  } :> IObject ]
                | :? CallExpression as ce -> let f = EvalRec env map ce.Function
                                             let args = InvokeEval env map EvalType.EXPRESSION (EvalParameters.ExpressionCollection ce.Arguments)
                                             InvokeEval env map EvalType.APPLY_FUNCTION (EvalParameters.ApplyFunction (f.[0], args))
                | :? ArrayLiteral as al -> let ele = InvokeEval env map EvalType.EXPRESSION (EvalParameters.ExpressionCollection al.Elements)
                                           [] @ [ { Array.Elements = ele |> List.toArray } :> IObject ]
                | :? IndexExpression as ie -> let left = EvalRec env map ie.Left
                                              let index = EvalRec env map ie.Index
                                              match index.[0] with
                                                | :? IHashableObject -> InvokeEval env map EvalType.HASH_INDEX_EXPRESSION (EvalParameters.HashIndexExpression (left.[0], index.[0] :?> IHashableObject))
                                                | _ -> InvokeEval env map EvalType.ARRAY_INDEX_EXPRESSION (EvalParameters.ArrayIndexExpression (left.[0], index.[0]))
                | :? HashLiteral as hl -> InvokeEval env map EvalType.HASH_LITERAL (EvalParameters.HashLiteral hl)
                | _ -> [] @ [ MonkeyNull ]
                
        let private IsErrorOrReturnObject (ele:IObject) =
            match ele with
                | :? ReturnValue -> true
                | :? Error -> true
                | _ -> false
        
        let private EvalProgram env map (node:EvalParameters) =
            match node with
            | EvalParameters.Program p -> let arr =  p.Statements
                                                        |> Array.map (fun s -> EvalRec env map s)
                                                      
                                          let arrFiltered = arr |> Array.filter(fun ele -> ele.[0] |> IsErrorOrReturnObject)
                                          match arrFiltered.Length with
                                                | 0 -> arr |> Array.last 
                                                | _ -> arrFiltered |> Array.head
            | _ -> [] @ [ MonkeyNull ]
        
        let private EvalBlockStatement env map (node:EvalParameters) =
             match node with
                | EvalParameters.BlockStatement bs -> let arr =  bs.Statements
                                                                    |> Array.map (fun s -> EvalRec env map s)
                                                      
                                                      let arrFiltered = arr |> Array.filter(fun ele -> ele.[0] |> IsErrorOrReturnObject)
                                                      match arrFiltered.Length with
                                                        | 0 -> arr |> Array.last 
                                                        | _ -> arrFiltered |> Array.head
                | _ -> [] @ [ MonkeyNull ]
        
        let private EvalBangOperatorExpression (right:IObject) =
            match right with
                | :? NULL -> [] @ [ MonkeyTrue ]
                | :? MonkeyObject.Boolean as b -> match b.Value with
                                                    | true -> [] @ [ MonkeyFalse ]
                                                    | false -> [] @ [ MonkeyTrue ]
                | _ -> [] @ [ MonkeyFalse ]
        
        let private EvalMinusPrefixOperatorExpression (right:IObject) =
            match right with
                | :? Integer as i -> [] @ [ ({ Integer.Value = -i.Value } :> IObject) ]
                | _ -> [] @ [ MonkeyNull ]
            
        let private EvalPrefixExpression env map (node:EvalParameters)  =
            match node with
                | EvalParameters.PrefixExpression (operator, right) -> match operator with
                                                                         | "!" -> EvalBangOperatorExpression right
                                                                         | "-" -> EvalMinusPrefixOperatorExpression right 
                                                                         | _ -> [] @ [ MonkeyNull ]
                | _ -> [] @ [ MonkeyNull ]
        
        let private EvalIntegerInfixExpression operator (left:Integer) (right:Integer) =
            match operator with
                | "+" -> [] @ [ { Integer.Value = (left.Value + right.Value)  } :> IObject ]
                | "-" -> [] @ [ { Integer.Value = (left.Value - right.Value)  } :> IObject ]
                | "/" -> [] @ [ { Integer.Value = (left.Value / right.Value)  } :> IObject ]
                | "*" -> [] @ [ { Integer.Value = (left.Value * right.Value)  } :> IObject ]
                | "<" -> [] @ [ BoolToMonkeyBool (left < right) ]
                | ">" -> [] @ [ BoolToMonkeyBool (left > right) ]
                | "==" -> [] @ [ BoolToMonkeyBool (left = right) ]
                | "!=" -> [] @ [ BoolToMonkeyBool (left <> right) ]
                | _ -> [] @ [ MonkeyNull ]
        
        let private EvalStringInfixExpression operator (left:MonkeyObject.String) (right:MonkeyObject.String) =
            match operator with
                | "+" -> [] @ [ { MonkeyObject.String.Value = (left.Value + right.Value)  } :> IObject ]
                | _ -> [] @ [ MonkeyNull ]
                
        let private EvalInfixExpression env map (node:EvalParameters) =
            match node with
                | EvalParameters.InfixExpression (operator, left, right) -> match operator with
                                                                             | "==" -> [] @ [ BoolToMonkeyBool (left = right) ]
                                                                             | "!=" -> [] @ [ BoolToMonkeyBool (left <> right) ]
                                                                             | _ -> match (left, right) with
                                                                                       | (:? Integer, :? Integer) as found -> let (l,r) = found
                                                                                                                              EvalIntegerInfixExpression operator (l :?> Integer) (r :?> Integer)
                                                                                       | (:? MonkeyObject.String, :? MonkeyObject.String) as found -> let (l,r) = found
                                                                                                                                                      EvalStringInfixExpression operator (l :?> MonkeyObject.String) (r :?> MonkeyObject.String)
                                                                                       | _ -> [] @ [ MonkeyNull ]
                | _ -> [] @ [ MonkeyNull ]
        
        let private IsTruthy (cond:IObject) = match cond with
                                                | :? NULL -> false
                                                | :? MonkeyObject.Boolean as b -> b.Value
                                                | _ -> false
                
        let private EvalIfExpression env map (node:EvalParameters) =
            match node with
                | EvalParameters.IfExpression n -> if (IsTruthy (EvalRec env map n.Condition).[0]) then
                                                       EvalRec env map n.Consequence
                                                       else match n.Alternative with
                                                                | Some alt -> EvalRec env map alt
                                                                | None -> [] @ [ MonkeyNull ]
                | _ -> [] @ [ MonkeyNull ]
                
        let private EvalIdentifier env map (node:EvalParameters) =
            match node with
                | EvalParameters.Identifier n -> match GetEnv n.Value env with
                                                    | Some v -> [] @ [ v ]
                                                    | None -> match BuiltInFuncs.TryFind(n.Value) with
                                                                | Some f -> [] @ [ f ]
                                                                | None ->  [] @ [ MonkeyNull ]
                | _ -> [] @ [ MonkeyNull ]
                
        let private EvalExpressions env map (node:EvalParameters) =
            match node with
                | EvalParameters.ExpressionCollection n -> n |> Array.map (fun e -> (EvalRec env map e).[0])
                                                           |> Array.toList
                | _ -> [] @ [ MonkeyNull ]
        
        let private ExtendFuncEnvironment env (f:Function) (args:IObject list) =
            let env' = NewClosedEnvironment env
            f.Parameters
            |> Array.zip (args |> List.toArray)
            |> Array.map (fun coll -> let (value,paramName) = coll
                                      SetEnv paramName.Value value env'  )
            |> ignore
            env'
        
        let private ApplyFunction env map (node:EvalParameters) =
            match node with
                | EvalParameters.ApplyFunction (fn, args) ->  match fn with
                                                                | :? Builtin as bif -> let result = bif.Fn.Invoke args
                                                                                       [] @ [ result ]
                                                                | :? Function  as f -> let env' = ExtendFuncEnvironment env f args
                                                                                       EvalRec env' map f.Body
                                                                | _ -> [] @ [ MonkeyNull ]
                | _ -> [] @ [ MonkeyNull ]
        
        let private EvalArrayIndexExpression (arr:MonkeyObject.Array) (idx:Integer) =
            match idx.Value with
                | l when (int)l < 0 -> [] @ [ MonkeyNull ]
                | h when (int)h > (arr.Elements.Length - 1) -> [] @ [ MonkeyNull ]
                | _ -> [] @ [ arr.Elements.[(int)idx.Value] ]
        
        let private EvalHashIndexExpression (hsh:Hash) (idx:IHashable) =
            match hsh.Pairs.TryFind(idx.HashKey()) with
                | Some p -> [] @ [ p.Value ]
                | None -> [] @ [ MonkeyNull ]
        
        let private EvalIndexExpression env map (node:EvalParameters) =
            match node with
                | EvalParameters.ArrayIndexExpression (left, index) ->  match left with
                                                                            | :? Array as a -> EvalArrayIndexExpression a (index :?> Integer)
                                                                            | _ -> [] @ [ MonkeyNull ]
                | EvalParameters.HashIndexExpression (left, index) ->  match left with
                                                                            | :? Hash  as h -> EvalHashIndexExpression h (index :> IHashable)
                                                                            | _ -> [] @ [ MonkeyNull ]                                              
                | _ -> [] @ [ MonkeyNull ]
        
        let private EvalHashLiteral env map (node:EvalParameters) =
            let pairs = Map.empty
            match node with
                | EvalParameters.HashLiteral hl ->  hl.Pair
                                                    |> Array.map(fun nd -> let (k, v) = nd
                                                                           let key = EvalRec env map k
                                                                           let value = EvalRec env map v
                                                                           pairs.Add((key.[0] :?> IHashableObject).HashKey(), {HashPair.Key = key.[0]
                                                                                                                               HashPair.Value = value.[0] }))
                                                    |> ignore
                                                    [] @ [ {Hash.Pairs = pairs} :> IObject]
                                        
                | _ -> [] @ [ MonkeyNull ]
                    
        let private EvalLookUpMap =
            Map.empty.Add(EvalType.PROGRAM, (EvalFn EvalProgram))
                .Add(EvalType.BLOCK_STATEMENT, (EvalFn EvalBlockStatement))
                .Add(EvalType.PREFIX_EXPRESSION, (EvalFn EvalPrefixExpression))
                .Add(EvalType.INFIX_EXPRESSION, (EvalFn EvalInfixExpression))
                .Add(EvalType.IF_EXPRESSION, (EvalFn EvalIfExpression))
                .Add(EvalType.IDENTIFIER, (EvalFn EvalIdentifier))
                .Add(EvalType.EXPRESSION, (EvalFn EvalExpressions))
                .Add(EvalType.APPLY_FUNCTION, (EvalFn ApplyFunction))
                .Add(EvalType.ARRAY_INDEX_EXPRESSION, (EvalFn EvalIndexExpression))
                .Add(EvalType.HASH_INDEX_EXPRESSION, (EvalFn EvalIndexExpression))
                .Add(EvalType.HASH_LITERAL, (EvalFn EvalHashLiteral))
             
        let Eval env node =
             (node |> EvalRec env EvalLookUpMap).[0]