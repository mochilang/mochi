open System

exception Break
exception Continue

type Anon1 = {
    cat: string
    ``val``: int
    flag: bool
}
type Anon2 = {
    cat: obj
    share: obj
}
let items = [{ cat = "a"; ``val`` = 10; flag = true }; { cat = "a"; ``val`` = 5; flag = false }; { cat = "b"; ``val`` = 20; flag = true }]
let result = [ for gKey, gItems in [ for i in items do yield i ] |> List.groupBy (fun i -> i.cat) |> List.sortBy (fun (gKey, gItems) -> let g = {| key = gKey; items = gItems |} in g.key) do
    let g = {| key = gKey; items = gItems |}
    yield { cat = g.key; share = List.sum [ for x in g do yield (if x.flag then x.val else 0) ] / List.sum [ for x in g do yield x.val ] } ]
printfn "%A" (result)
