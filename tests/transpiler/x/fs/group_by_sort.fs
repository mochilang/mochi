// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    cat: string
    val: int
}
type Anon2 = {
    cat: string
    val: int
}
type Anon3 = {
    cat: obj
    total: float
}
type Anon4 = {
    i: Anon2
}
type Anon5 = {
    key: string
    items: Anon4 list
}
type Anon6 = {
    cat: obj
    total: float
}
let items: Anon2 list = [{ cat = "a"; val = 3 }; { cat = "a"; val = 1 }; { cat = "b"; val = 5 }; { cat = "b"; val = 2 }]
let grouped: Anon6 list = [ for (key, items) in List.groupBy (fun i -> i.cat) items do
    let g : Anon5 = { key = key; items = items }
    yield { cat = g.key; total = List.sum [ for x in g.items do yield x.val ] } ]
printfn "%s" (("[" + (String.concat ", " (List.map string grouped))) + "]")
