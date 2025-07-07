type person = { name: string; age: int }

type book = { title: string; author: person }

let book = { title = "Go"; author = { name = "Bob"; age = 42 } }

let () =
  print_endline book.author.name
