namespace MonkeyInterpreter

    module MonkeyObject =
        
        open System
        open AST
        
        type ObjectType =
            | NULL
            | ERROR
            | INTEGER
            | BOOLEAN
            | STRING
            | RETURNVAL
            | FUNCTION
            | BUILTIN
            | ARRAY
            | HASH
        
        type HashKey =
            { Type : ObjectType
              Value: uint64 }
        
        type IHashable =
            abstract member HashKey : unit -> HashKey
            
        type IObject =
            abstract member Type : unit -> ObjectType
            abstract member Inspect : unit -> string
        and MonkeyEnvironment =
            { Store : Map<string, IObject>
              Outer : Option<MonkeyEnvironment> }
            
        type BuiltInFunction = delegate of list<IObject> -> IObject
            
        type IHashableObject =
            inherit IHashable
            inherit IObject

        type Integer =
            { Value: int64 }
            interface IHashableObject with
                member this.Type() = ObjectType.INTEGER
                member this.Inspect() = String.Format("{0}", this.Value)
                member this.HashKey() = { Type = (this :> IObject).Type()
                                          Value = (uint64)this.Value }
        
        type Boolean =
            { Value: bool }
            interface IHashableObject with
                member this.Type() = ObjectType.BOOLEAN
                member this.Inspect() = String.Format("{0}", this.Value)
                member this.HashKey() = { Type = (this :> IObject).Type()
                                          Value = if this.Value then 1UL else 0UL }
                
        type NUll =
            interface IObject with
                member this.Type() = ObjectType.NULL
                member this.Inspect() = "null"
        
        type ReturnValue =
            { Value: IObject }
            interface IObject with
                member this.Type() = ObjectType.RETURNVAL
                member this.Inspect() = this.Value.Inspect()
        
        type Error =
            { Message: string }
            interface IObject with
                member this.Type() = ObjectType.ERROR
                member this.Inspect() = String.Format("ERROR: {0}", this.Message)
        
        type Function =
            { Parameters : Identifier[]
              Body : BlockStatement
              Env : MonkeyEnvironment }
            
            interface IObject with
                member this.Type() = ObjectType.FUNCTION
                member this.Inspect() =
                    let paramStr = this.Parameters
                                        |> Array.map (fun p -> p.ToString())
                                        |> String.concat ", "
                    String.Format("fn({0}){{1}{2}{3}}", paramStr,Environment.NewLine, this.Body.ToString(), Environment.NewLine)
        
        type String =
            { Value : string }
            interface IHashableObject with
                member this.Type() = ObjectType.STRING
                member this.Inspect() = this.Value
                member this.HashKey() =
                        // TODO this is a shortcut
                        // create a proper hash generator in future
                        // for now relying on .net
                        let hashCode = this.Value.GetHashCode()
                        { Type = (this :> IObject).Type()
                          Value = (uint64)hashCode }
        
        type Builtin =
            { Fn : BuiltInFunction }
            interface IObject with
                member this.Type() = ObjectType.BUILTIN
                member this.Inspect() = "builtin function"
                
        type Array =
            { Element : IObject[] }
            interface IObject with
                member this.Type() = ObjectType.ARRAY
                member this.Inspect() =
                    let eleStr = this.Element
                                    |> Array.map (fun p -> p.ToString())
                                    |> String.concat ", "
                                        
                    String.Format("[{0}]", eleStr)
        
        type HashPair =
            { Key : IObject
              Value : IObject }
        
        type Hash =
            { Pairs : Map<HashKey, HashPair> }
            interface IObject with
                member this.Type() = ObjectType.HASH
                member this.Inspect() =
                    let keyPair = this.Pairs
                                    |> Map.toArray
                                    |> Array.map (fun (k,p) -> String.Format("{0}:{1}", p.Key.Inspect(), p.Value.Inspect()))
                                    |> String.concat ", "
                                        
                    String.Format("{{{0}}}", keyPair)   