// Generated 2025-07-20 22:28 +0700
open System

type Todo = {
    title: string
}

let todo = ({ title = "hi" } : Todo)
printfn "%s" (string (todo.title))
