// Generated 2025-08-01 18:27 +0700

exception Return

type SumCarry = {
    s: bool
    c: bool
}
type Add4Result = {
    v: bool
    s3: bool
    s2: bool
    s1: bool
    s0: bool
}
let rec xor (a: bool) (b: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (a && (not b)) || ((not a) && b)
        raise Return
        __ret
    with
        | Return -> __ret
and ha (a: bool) (b: bool) =
    let mutable __ret : SumCarry = Unchecked.defaultof<SumCarry>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { s = xor a b; c = a && b }
        raise Return
        __ret
    with
        | Return -> __ret
and fa (a: bool) (b: bool) (c0: bool) =
    let mutable __ret : SumCarry = Unchecked.defaultof<SumCarry>
    let mutable a = a
    let mutable b = b
    let mutable c0 = c0
    try
        let r1: SumCarry = ha a c0
        let r2: SumCarry = ha (r1.s) b
        __ret <- { s = r2.s; c = (r1.c) || (r2.c) }
        raise Return
        __ret
    with
        | Return -> __ret
and add4 (a3: bool) (a2: bool) (a1: bool) (a0: bool) (b3: bool) (b2: bool) (b1: bool) (b0: bool) =
    let mutable __ret : Add4Result = Unchecked.defaultof<Add4Result>
    let mutable a3 = a3
    let mutable a2 = a2
    let mutable a1 = a1
    let mutable a0 = a0
    let mutable b3 = b3
    let mutable b2 = b2
    let mutable b1 = b1
    let mutable b0 = b0
    try
        let r0: SumCarry = fa a0 b0 false
        let r1: SumCarry = fa a1 b1 (r0.c)
        let r2: SumCarry = fa a2 b2 (r1.c)
        let r3: SumCarry = fa a3 b3 (r2.c)
        __ret <- { v = r3.c; s3 = r3.s; s2 = r2.s; s1 = r1.s; s0 = r0.s }
        raise Return
        __ret
    with
        | Return -> __ret
and b2i (b: bool) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    try
        __ret <- if b then 1 else 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let r: Add4Result = add4 true false true false true false false true
        printfn "%s" (((((((((string (b2i (r.v))) + " ") + (string (b2i (r.s3)))) + " ") + (string (b2i (r.s2)))) + " ") + (string (b2i (r.s1)))) + " ") + (string (b2i (r.s0))))
        __ret
    with
        | Return -> __ret
main()
