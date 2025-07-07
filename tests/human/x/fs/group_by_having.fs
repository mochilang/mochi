open System
open System.Text.Json

type Person = { name: string; city: string }

let people = [
    { name = "Alice"; city = "Paris" }
    { name = "Bob"; city = "Hanoi" }
    { name = "Charlie"; city = "Paris" }
    { name = "Diana"; city = "Hanoi" }
    { name = "Eve"; city = "Paris" }
    { name = "Frank"; city = "Hanoi" }
    { name = "George"; city = "Paris" }
]

let big =
    people
    |> List.groupBy (fun p -> p.city)
    |> List.filter (fun (_, grp) -> List.length grp >= 4)
    |> List.map (fun (city, grp) -> {| city = city; num = List.length grp |})

let jsonStr = JsonSerializer.Serialize(big)
printfn "%s" jsonStr
