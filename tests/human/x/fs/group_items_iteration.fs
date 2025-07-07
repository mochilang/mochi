open System

type Record = { tag: string; value: int }

let data = [
    { tag = "a"; value = 1 }
    { tag = "a"; value = 2 }
    { tag = "b"; value = 3 }
]

let groups = data |> List.groupBy (fun d -> d.tag)

let tmp =
    [ for (tag, items) in groups do
        let total = items |> List.sumBy (fun x -> x.value)
        yield {| tag = tag; total = total |} ]
    |> List.sortBy (fun x -> x.tag)

printfn "%A" tmp
