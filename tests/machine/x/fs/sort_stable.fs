open System

exception Break
exception Continue

type Anon1 = {
    n: int
    v: string
}
let items = [{ n = 1; v = "a" }; { n = 1; v = "b" }; { n = 2; v = "c" }]
let result = [ for i in items do yield i.v ] |> List.sortBy (fun i -> i.n)
printfn "%A" (result)
