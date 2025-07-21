// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable a: int
    mutable b: int
    mutable c: int
}
let m: Anon1 = { a = 1; b = 2; c = 3 }
printfn "%s" (string (Seq.map snd m))
