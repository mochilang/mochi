open System

exception Break
exception Continue

type Todo = {
    title: string
}
let todo = { title = "hi" }
printfn "%s" (string todo.title)
