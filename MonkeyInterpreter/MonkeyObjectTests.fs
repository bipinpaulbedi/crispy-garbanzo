namespace MonkeyInterpreter
    module MonkeyObjectTests =
        
        open FsUnit
        open Xunit
        open MonkeyObject
        
        [<Theory>]
        [<InlineData("Same|Same|Diff|Diff")>]  
        let ``Test String Hash Key`` (inp:string) =
            let coll = inp.Split "|"
                            |> Array.map (fun s -> { Value = s })
            
            (coll.[0] :> IHashable).HashKey() |> should equal ((coll.[1] :> IHashable).HashKey())
            (coll.[2] :> IHashable).HashKey() |> should equal ((coll.[3] :> IHashable).HashKey())
            (coll.[0] :> IHashable).HashKey() |> should not' (equal ((coll.[2] :> IHashable).HashKey()))
        
        [<Theory>]
        [<InlineData("true|true|false|false")>]  
        let ``Test Boolean Hash Key`` (inp:string) =
            let coll = inp.Split "|"
                            |> Array.map (fun s -> bool.Parse(s) )
                            |> Array.map (fun s -> { Boolean.Value = s })
            
            (coll.[0] :> IHashable).HashKey() |> should equal ((coll.[1] :> IHashable).HashKey())
            (coll.[2] :> IHashable).HashKey() |> should equal ((coll.[3] :> IHashable).HashKey())
            (coll.[0] :> IHashable).HashKey() |> should not' (equal ((coll.[2] :> IHashable).HashKey()))
            
        [<Theory>]
        [<InlineData("1|1|2|2")>]  
        let ``Test Integer Hash Key`` (inp:string) =
            let coll = inp.Split "|"
                            |> Array.map (fun s -> (int64)s )
                            |> Array.map (fun v -> { Integer.Value = v })
                                          
                                          
            
            (coll.[0] :> IHashable).HashKey() |> should equal ((coll.[1] :> IHashable).HashKey())
            (coll.[2] :> IHashable).HashKey() |> should equal ((coll.[3] :> IHashable).HashKey())
            (coll.[0] :> IHashable).HashKey() |> should not' (equal ((coll.[2] :> IHashable).HashKey()))
            
            

