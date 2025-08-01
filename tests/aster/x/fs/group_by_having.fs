// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    name: string
    city: string
}
type Anon2 = {
    name: string
    city: string
}
type Anon3 = {
    city: obj
    num: int
}
type Anon4 = {
    p: Anon2
}
type Anon5 = {
    key: string
    items: Anon4 list
}
type Anon6 = {
    city: obj
    num: int
}
let people: Anon2 list = [{ name = "Alice"; city = "Paris" }; { name = "Bob"; city = "Hanoi" }; { name = "Charlie"; city = "Paris" }; { name = "Diana"; city = "Hanoi" }; { name = "Eve"; city = "Paris" }; { name = "Frank"; city = "Hanoi" }; { name = "George"; city = "Paris" }]
let big: Anon6 list = [ for (key, items) in List.groupBy (fun p -> p.city) people do
    let g : Anon5 = { key = key; items = items }
    yield { city = g.key; num = List.length (g.items) } ]
json big
