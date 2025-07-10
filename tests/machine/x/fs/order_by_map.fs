open System

type Anon1 = {
    a: int
    b: int
}
type Anon2 = {
    a: obj
    b: obj
}
let data = [{ a = 1; b = 2 }; { a = 1; b = 1 }; { a = 0; b = 5 }]
let sorted = [ for x in data do yield x ] |> List.sortBy (fun x -> { a = x.a; b = x.b })
printfn "%A" (sorted)
