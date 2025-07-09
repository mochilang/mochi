open System

exception Break
exception Continue

type Todo = {
    title: string
}
let todo: Todo = { title = "hi" }
printfn "%s" todo.title
