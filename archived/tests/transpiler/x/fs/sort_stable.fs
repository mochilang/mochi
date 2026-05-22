// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable n: int
    mutable v: string
}
type Anon2 = {
    mutable n: int
    mutable v: string
}
let items: Anon2 list = [{ n = 1; v = "a" }; { n = 1; v = "b" }; { n = 2; v = "c" }]
let result = [ for i in List.sortBy (fun i -> (i.n)) items do yield i.v ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")
