open System

type Item = { cat: string; value: int; flag: bool }

let items = [
    { cat = "a"; value = 10; flag = true }
    { cat = "a"; value = 5; flag = false }
    { cat = "b"; value = 20; flag = true }
]

let result =
    items
    |> List.groupBy (fun i -> i.cat)
    |> List.sortBy fst
    |> List.map (fun (cat, grp) ->
        let share =
            (grp |> List.sumBy (fun x -> if x.flag then x.value else 0) |> float) /
            (grp |> List.sumBy (fun x -> x.value) |> float)
        {| cat = cat; share = share |})

printfn "%A" result
