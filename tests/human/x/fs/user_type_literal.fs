open System

type Person = { name: string; age: int }
type Book = { title: string; author: Person }

let book = { title = "Go"; author = { name = "Bob"; age = 42 } }
printfn "%s" book.author.name
