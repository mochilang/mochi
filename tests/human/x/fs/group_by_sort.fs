open System

type Item = { cat: string; value: int }

let items = [
    { cat = "a"; value = 3 }
    { cat = "a"; value = 1 }
    { cat = "b"; value = 5 }
    { cat = "b"; value = 2 }
]

let grouped =
    items
    |> List.groupBy (fun i -> i.cat)
    |> List.sortByDescending (fun (_, grp) -> grp |> List.sumBy (fun x -> x.value))
    |> List.map (fun (cat, grp) ->
        {| cat = cat; total = grp |> List.sumBy (fun x -> x.value) |})

printfn "%A" grouped
