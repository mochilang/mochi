// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable tag: string
    mutable val: int
}
type Anon2 = {
    mutable tag: string
    mutable val: int
}
type Anon3 = {
    mutable d: obj
}
type Anon4 = {
    mutable key: obj
    mutable items: Anon3 list
}
type Anon5 = {
    mutable tag: obj
    mutable total: obj
}
let data: Anon2 list = [{ tag = "a"; val = 1 }; { tag = "a"; val = 2 }; { tag = "b"; val = 3 }]
let groups = [ for (key, items) in List.groupBy (fun d -> d.tag) data do
    let g : Anon4 = { key = key; items = items }
    yield g ]
let mutable tmp = []
for g in groups do
let mutable total: int = 0
for x in g.items do
total <- total + (x.val)
tmp <- tmp @ [{ tag = g.key; total = total }]
let result = [ for r in List.sortBy (fun r -> (r.tag)) tmp do yield r ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")
