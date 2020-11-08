namespace MonkeyInterpreter

    open MonkeyInterpreter

    module Program =
        open MonkeyEnvironment
        
        [<EntryPoint>]
        
        printfn "Hello! This is Monkey programming language"
        printfn "Feel free to type commands"
        
        Repl.start(NewEnvironment None)
