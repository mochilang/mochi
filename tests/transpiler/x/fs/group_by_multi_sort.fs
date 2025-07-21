// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    a: string
    b: int
    val: int
}
type Anon2 = {
    a: string
    b: int
    val: int
}
type Anon3 = {
    a: obj
    b: obj
}
type Anon4 = {
    a: obj
    b: obj
    total: float
}
type Anon5 = {
    i: Anon2
}
type Anon6 = {
    key: Anon3
    items: Anon5 list
}
type Anon7 = {
    a: obj
    b: obj
    total: float
}
let items: Anon2 list = [{ a = "x"; b = 1; val = 2 }; { a = "x"; b = 2; val = 3 }; { a = "y"; b = 1; val = 4 }; { a = "y"; b = 2; val = 1 }]
let grouped: Anon7 list = [ for (key, items) in List.groupBy (fun i -> { a = i.a; b = i.b }) items do
    let g : Anon6 = { key = key; items = items }
    yield { a = g.key.a; b = g.key.b; total = List.sum [ for x in g.items do yield x.val ] } ]
printfn "%s" (("[" + (String.concat ", " (List.map string grouped))) + "]")
