// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable outer: Map<string, int>
}
type Anon2 = {
    mutable inner: int
}
let mutable data: Anon1 = { outer = { inner = 1 } }
data.["outer"].["inner"] <- 2
printfn "%s" (string (data.["outer"].["inner"]))
