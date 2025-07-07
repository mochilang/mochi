open System

type Person = { name: string; age: int; city: string }

let people = [
    { name = "Alice"; age = 30; city = "Paris" }
    { name = "Bob"; age = 15; city = "Hanoi" }
    { name = "Charlie"; age = 65; city = "Paris" }
    { name = "Diana"; age = 45; city = "Hanoi" }
    { name = "Eve"; age = 70; city = "Paris" }
    { name = "Frank"; age = 22; city = "Hanoi" }
]

let stats =
    people
    |> List.groupBy (fun p -> p.city)
    |> List.map (fun (city, persons) ->
        let count = List.length persons
        let avg_age = persons |> List.averageBy (fun p -> float p.age)
        {| city = city; count = count; avg_age = avg_age |})

printfn "--- People grouped by city ---"
for s in stats do
    printfn "%s: count = %d, avg_age = %f" s.city s.count s.avg_age
