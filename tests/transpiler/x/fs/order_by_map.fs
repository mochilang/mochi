// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable a: int
    mutable b: int
}
type Anon2 = {
    mutable a: int
    mutable b: int
}
type Anon3 = {
    mutable a: obj
    mutable b: obj
}
let data: Anon2 list = [{ a = 1; b = 2 }; { a = 1; b = 1 }; { a = 0; b = 5 }]
let sorted = [ for x in List.sortBy (fun x -> { a = x.a; b = x.b }) data do yield x ]
printfn "%s" (("[" + (String.concat ", " (List.map string sorted))) + "]")
