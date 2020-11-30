namespace MonkeyInterpreter

    open System.Collections.Generic

    module MonkeyEnvironment =
        
        open MonkeyObject
        
        let NewEnvironment environment =
            match environment with
             | Some e ->  { Store = Dictionary<string, IObject>()
                            Outer = Some e }
             | _ -> { Store = Dictionary<string, IObject>()
                      Outer = None }
             
        let NewClosedEnvironment environment =
            NewEnvironment  (Some environment)
            
        let GetEnv name env =
            match env.Store.ContainsKey name with
             | true -> Some (env.Store.Item name)
             | _ -> match env.Outer with
                        | Some env' -> match env'.Store.ContainsKey name with
                                            | true -> Some (env.Store.Item name)
                                            | _ -> None
                        | _ -> None
        
        let SetEnv name value env =
            env.Store.Add(name, value) |> ignore
                                            
            

