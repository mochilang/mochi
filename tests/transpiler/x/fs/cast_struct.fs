// Generated 2025-07-21 15:37 +0700
open System

type Todo = {
    mutable title: string
}
type Anon1 = {
    mutable title: string
}
let todo = { title = "hi" }
printfn "%s" (string (todo.title))
