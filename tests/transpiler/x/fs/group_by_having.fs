// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable name: string
    mutable city: string
}
type Anon2 = {
    mutable name: string
    mutable city: string
}
type Anon3 = {
    mutable city: obj
    mutable num: int
}
type Anon4 = {
    mutable p: obj
}
type Anon5 = {
    mutable key: obj
    mutable items: Anon4 list
}
type Anon6 = {
    mutable city: obj
    mutable num: int
}
let people: Anon2 list = [{ name = "Alice"; city = "Paris" }; { name = "Bob"; city = "Hanoi" }; { name = "Charlie"; city = "Paris" }; { name = "Diana"; city = "Hanoi" }; { name = "Eve"; city = "Paris" }; { name = "Frank"; city = "Hanoi" }; { name = "George"; city = "Paris" }]
let big: Anon6 list = [ for (key, items) in List.groupBy (fun p -> p.city) people do
    let g : Anon5 = { key = key; items = items }
    yield { city = g.key; num = List.length (g.items) } ]
json big
