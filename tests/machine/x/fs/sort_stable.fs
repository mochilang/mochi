open System

type Anon1 = {
    n: int
    v: string
}
let items: obj list = [{ n = 1; v = "a" }; { n = 1; v = "b" }; { n = 2; v = "c" }]
let result: obj list = [ for i in items do yield i.v ] |> List.sortBy (fun i -> i.n)
printfn "%A" (result)
