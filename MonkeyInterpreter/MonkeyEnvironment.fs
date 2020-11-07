namespace MonkeyInterpreter
    module MonkeyEnvironment =
        
        open MonkeyObject
        
        let NewEnvironment environment =
            match environment with
             | Some e ->  { Store = Map.empty
                            Outer = Some e }
             | _ -> { Store = Map.empty
                      Outer = None }
             
        let NewClosedEnvironment environment =
            NewEnvironment  (Some environment)
            
        let GetEnv name env =
            match env.Store.TryFind name with
             | Some v -> Some v
             | _ -> match env.Outer with
                        | Some env' -> match env'.Store.TryFind name with
                                            | Some v' -> Some v'
                                            | _ -> None
                        | _ -> None
        
        let SetEnv name value env =
            env.Store.Add(name, value) |> ignore
                                            
            

