// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    cat: string
    val: int
    flag: bool
}
type Anon2 = {
    cat: string
    val: int
    flag: bool
}
type Anon3 = {
    cat: obj
    share: float
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
    share: float
}
let items: Anon2 list = [{ cat = "a"; val = 10; flag = true }; { cat = "a"; val = 5; flag = false }; { cat = "b"; val = 20; flag = true }]
let result: Anon6 list = [ for (key, items) in List.groupBy (fun i -> i.cat) items do
    let g : Anon5 = { key = key; items = items }
    yield { cat = g.key; share = (List.sum [ for x in g.items do yield if x.flag then (x.val) else 0 ]) / (List.sum [ for x in g.items do yield x.val ]) } ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")
