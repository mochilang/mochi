open System
open System.Text.Json

exception Break
exception Continue

type Anon1 = {
    name: string
    city: string
}
type Anon2 = {
    city: obj
    num: obj
}
let people = [{ name = "Alice"; city = "Paris" }; { name = "Bob"; city = "Hanoi" }; { name = "Charlie"; city = "Paris" }; { name = "Diana"; city = "Hanoi" }; { name = "Eve"; city = "Paris" }; { name = "Frank"; city = "Hanoi" }; { name = "George"; city = "Paris" }]
let big = [ for gKey, gItems in [ for p in people do yield p ] |> List.groupBy (fun p -> p.city) |> List.filter (fun (gKey, gItems) -> let g = {| key = gKey; items = gItems |} in List.length g >= 4) do
    let g = {| key = gKey; items = gItems |}
    yield { city = g.key; num = List.length g } ]
printfn "%A" (JsonSerializer.Serialize(big))

