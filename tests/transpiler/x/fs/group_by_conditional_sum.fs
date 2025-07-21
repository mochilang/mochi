// Generated 2025-07-21 15:37 +0700
open System

type Anon1 = {
    mutable cat: string
    mutable val: int
    mutable flag: bool
}
type Anon2 = {
    mutable cat: string
    mutable val: int
    mutable flag: bool
}
type Anon3 = {
    mutable cat: obj
    mutable share: float
}
type Anon4 = {
    mutable cat: obj
    mutable share: float
}
let items: Anon2 list = [{ cat = "a"; val = 10; flag = true }; { cat = "a"; val = 5; flag = false }; { cat = "b"; val = 20; flag = true }]
let result: Anon4 list = [ for (key, items) in List.groupBy (fun i -> i.cat) items do
    let g = {| key = key; items = items |}
    yield { cat = g.key; share = (List.sum [ for x in g.items do yield if x.flag then (x.val) else 0 ]) / (List.sum [ for x in g.items do yield x.val ]) } ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")
