open System

type Anon1 = {
    cat: string
    ``val``: int
}
type Anon2 = {
    cat: obj
    total: obj
}
let items: obj list = [{ cat = "a"; ``val`` = 3 }; { cat = "a"; ``val`` = 1 }; { cat = "b"; ``val`` = 5 }; { cat = "b"; ``val`` = 2 }]
let grouped: obj list = [ for gKey, gItems in [ for i in items do yield i ] |> List.groupBy (fun i -> i.cat) |> List.sortByDescending (fun (gKey, gItems) -> let g = {| key = gKey; items = gItems |} in List.sum [ for x in g do yield x.val ]) do
    let g = {| key = gKey; items = gItems |}
    yield { cat = g.key; total = List.sum [ for x in g do yield x.val ] } ]
printfn "%A" (grouped)
