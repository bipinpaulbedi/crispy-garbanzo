namespace MonkeyInterpreter

    open System.Collections.Generic

    module BuiltIns =
        
        open MonkeyObject
        let private MonkeyNull = Unchecked.defaultof<NULL> :> IObject
        
        let private Len (inp:list<IObject>) : IObject =
            match inp.Length with
            | 1 -> match inp.[0] with
                    | :? MonkeyObject.Array as arr -> { Integer.Value = int64(arr.Elements.Length) } :> IObject
                    | :? MonkeyObject.String as str -> { Integer.Value = int64(str.Value.Length) } :> IObject
                    | _ -> MonkeyNull
            | _ -> MonkeyNull
        
        let private Puts (inp:list<IObject>) : IObject =
            inp
            |> List.map(fun ele -> printfn "%s" (ele.Inspect()))
            |> ignore
            
            MonkeyNull
            
        
        let private First (inp:list<IObject>) : IObject =
            match inp.Length with
            | 1 -> match inp.[0] with
                    | :? MonkeyObject.Array as arr -> match arr.Elements.Length with
                                                        | v when v > 0 -> arr.Elements.[0]
                                                        | _ -> MonkeyNull
                    | _ -> MonkeyNull
            | _ -> MonkeyNull
        
        let private Last (inp:list<IObject>) : IObject =
            match inp.Length with
            | 1 -> match inp.[0] with
                    | :? MonkeyObject.Array as arr -> match arr.Elements.Length with
                                                        | v when v > 0 -> arr.Elements.[arr.Elements.Length - 1]
                                                        | _ -> MonkeyNull
                    | _ -> MonkeyNull
            | _ -> MonkeyNull
        
        let private Rest (inp:list<IObject>) : IObject =
            match inp.Length with
            | 1 -> match inp.[0] with
                    | :? MonkeyObject.Array as arr -> match arr.Elements.Length with
                                                        | v when v > 0 ->
                                                            { Array.Elements = arr.Elements.[1..arr.Elements.Length - 1] } :> IObject
                                                        | _ -> MonkeyNull
                    | _ -> MonkeyNull
            | _ -> MonkeyNull
        
        let private Push (inp:list<IObject>) : IObject =
            match inp.Length with
            | 2 -> match inp.[0] with
                    | :? MonkeyObject.Array as arr -> match arr.Elements.Length with
                                                        | v when v > 0 ->
                                                            { Array.Elements = (arr.Elements |> Array.toList)  @ [ inp.[1]] |> List.toArray } :> IObject
                                                        | _ -> MonkeyNull
                    | _ -> MonkeyNull
            | _ -> MonkeyNull
    
        let BuiltInFuncs : Map<string, Builtin> =
            Map.empty
                .Add("len", { Builtin.Fn = BuiltInFunction Len})
                .Add("puts", { Builtin.Fn = BuiltInFunction Puts})
                .Add("first", { Builtin.Fn = BuiltInFunction First})
                .Add("last", { Builtin.Fn = BuiltInFunction Last})
                .Add("rest", { Builtin.Fn = BuiltInFunction Rest})
                .Add("push", { Builtin.Fn = BuiltInFunction Push})

