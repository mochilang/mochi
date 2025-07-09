open System

exception Break
exception Continue

type Anon1 = {
    tag: string
    val: int
}
type Anon2 = {
    tag: obj
    total: obj
}
let data = [{ tag = "a"; val = 1 }; { tag = "a"; val = 2 }; { tag = "b"; val = 3 }]
let groups = [ for gKey, gItems in [ for d in data do yield d ] |> List.groupBy (fun d -> d.tag) do
    let g = {| key = gKey; items = gItems |}
    yield g ]
let mutable tmp = [||]
try
    for g in groups do
        try
            let mutable total = 0
            try
                for x in g.items do
                    try
                        total <- total + x.val
                    with Continue -> ()
            with Break -> ()
            tmp <- tmp @ [{ tag = g.key; total = total }]
        with Continue -> ()
with Break -> ()
let result = [ for r in tmp do yield r ] |> List.sortBy (fun r -> r.tag)
printfn "%A" (result)
