open System

type Anon1 = {
    tag: string
    ``val``: int
}
type Anon2 = {
    tag: obj
    total: int
}
let data: obj list = [{ tag = "a"; ``val`` = 1 }; { tag = "a"; ``val`` = 2 }; { tag = "b"; ``val`` = 3 }]
let groups: obj list = [ for gKey, gItems in [ for d in data do yield d ] |> List.groupBy (fun d -> d.tag) do
    let g = {| key = gKey; items = gItems |}
    yield g ]
let mutable tmp: obj = [||]
for g in groups do
    let mutable total: int = 0
    for x in g.items do
        total <- total + x.val
    tmp <- tmp @ [{ tag = g.key; total = total }]
let result: obj list = [ for r in tmp do yield r ] |> List.sortBy (fun r -> r.tag)
printfn "%A" (result)
