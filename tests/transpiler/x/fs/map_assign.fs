// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable alice: int
}
let mutable scores: Anon1 = { alice = 1 }
scores.["bob"] <- 2
printfn "%s" (string (scores.["bob"]))
