
type Todo = {
    mutable title: string
}
let todo: Todo = { title = "hi" }
printfn "%s" todo.title
