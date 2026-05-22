// Generated 2025-07-21 18:37 +0700

type Person = {
    mutable name: string
    mutable age: int
}
type Book = {
    mutable title: string
    mutable author: Person
}
let book: Book = { title = "Go"; author = { name = "Bob"; age = 42 } }
printfn "%s" (string (book.author.name))
