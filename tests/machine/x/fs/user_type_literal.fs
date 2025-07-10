open System

type Person = {
    mutable name: string
    mutable age: int
}
type Book = {
    mutable title: string
    mutable author: Person
}
let book = { title = "Go"; author = { name = "Bob"; age = 42 } }
printfn "%A" (book.author.name)
