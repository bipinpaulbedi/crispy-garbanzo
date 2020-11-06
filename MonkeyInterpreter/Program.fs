namespace MonkeyInterpreter
    module Program =
        [<EntryPoint>]
        
        printfn "Hello! This is Monkey programming language"
        printfn "Feel free to type commands"
        
        Repl.start()
