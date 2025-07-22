// Generated 2025-07-22 08:52 +0700

type Anon1 = {
    a: int
    b: int
    c: int
}
let m: Anon1 = { a = 1; b = 2; c = 3 }
printfn "%s" (("[" + (String.concat ", " (List.map string [m.a; m.b; m.c]))) + "]")
