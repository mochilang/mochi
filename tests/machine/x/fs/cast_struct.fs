open System

type Todo = {
    mutable title: string
}
let todo: System.Collections.Generic.IDictionary<string, string> = { title = "hi" }
printfn "%A" (todo.title)
