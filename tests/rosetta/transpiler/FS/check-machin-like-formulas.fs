// Generated 2025-07-27 23:45 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
type Anon1 = {
    a: int
    n: int
    d: int
}
type Anon2 = {
    a: int
    n: int
    d: int
}
type Anon3 = {
    a: int
    n: int
    d: int
}
type Anon4 = {
    a: int
    n: int
    d: int
}
type Anon5 = {
    a: int
    n: int
    d: int
}
type Anon6 = {
    a: int
    n: int
    d: int
}
type Anon7 = {
    a: int
    n: int
    d: int
}
type Anon8 = {
    a: int
    n: int
    d: int
}
type Anon9 = {
    a: int
    n: int
    d: int
}
type Anon10 = {
    a: int
    n: int
    d: int
}
type Anon11 = {
    a: int
    n: int
    d: int
}
type Anon12 = {
    a: int
    n: int
    d: int
}
type Anon13 = {
    a: int
    n: int
    d: int
}
type Anon14 = {
    a: int
    n: int
    d: int
}
type Anon15 = {
    a: int
    n: int
    d: int
}
type Anon16 = {
    a: int
    n: int
    d: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec br (n: int) (d: int) =
    let mutable __ret : bignum = Unchecked.defaultof<bignum>
    let mutable n = n
    let mutable d = d
    try
        __ret <- (unbox<bignum> n) / (unbox<bignum> (unbox<bignum> d))
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testCases: struct array array = [|[|{ a = 1; n = 1; d = 2 }; { a = 1; n = 1; d = 3 }|]; [|{ a = 2; n = 1; d = 3 }; { a = 1; n = 1; d = 7 }|]; [|{ a = 4; n = 1; d = 5 }; { a = -1; n = 1; d = 239 }|]; [|{ a = 5; n = 1; d = 7 }; { a = 2; n = 3; d = 79 }|]; [|{ a = 1; n = 1; d = 2 }; { a = 1; n = 1; d = 5 }; { a = 1; n = 1; d = 8 }|]; [|{ a = 4; n = 1; d = 5 }; { a = -1; n = 1; d = 70 }; { a = 1; n = 1; d = 99 }|]; [|{ a = 5; n = 1; d = 7 }; { a = 4; n = 1; d = 53 }; { a = 2; n = 1; d = 4443 }|]; [|{ a = 6; n = 1; d = 8 }; { a = 2; n = 1; d = 57 }; { a = 1; n = 1; d = 239 }|]; [|{ a = 8; n = 1; d = 10 }; { a = -1; n = 1; d = 239 }; { a = -4; n = 1; d = 515 }|]; [|{ a = 12; n = 1; d = 18 }; { a = 8; n = 1; d = 57 }; { a = -5; n = 1; d = 239 }|]; [|{ a = 16; n = 1; d = 21 }; { a = 3; n = 1; d = 239 }; { a = 4; n = 3; d = 1042 }|]; [|{ a = 22; n = 1; d = 28 }; { a = 2; n = 1; d = 443 }; { a = -5; n = 1; d = 1393 }; { a = -10; n = 1; d = 11018 }|]; [|{ a = 22; n = 1; d = 38 }; { a = 17; n = 7; d = 601 }; { a = 10; n = 7; d = 8149 }|]; [|{ a = 44; n = 1; d = 57 }; { a = 7; n = 1; d = 239 }; { a = -12; n = 1; d = 682 }; { a = 24; n = 1; d = 12943 }|]; [|{ a = 88; n = 1; d = 172 }; { a = 51; n = 1; d = 239 }; { a = 32; n = 1; d = 682 }; { a = 44; n = 1; d = 5357 }; { a = 68; n = 1; d = 12943 }|]; [|{ a = 88; n = 1; d = 172 }; { a = 51; n = 1; d = 239 }; { a = 32; n = 1; d = 682 }; { a = 44; n = 1; d = 5357 }; { a = 68; n = 1; d = 12944 }|]|]
let rec format (ts: Map<string, int> array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ts = ts
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length ts)) do
            let t: Map<string, int> = ts.[i]
            s <- ((((((s + "{") + (string (t.["a"] |> unbox<int>))) + " ") + (string (t.["n"] |> unbox<int>))) + " ") + (string (t.["d"] |> unbox<int>))) + "}"
            if i < (unbox<int> ((unbox<int> (Array.length ts)) - 1)) then
                s <- s + " "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec tanEval (coef: int) (f: bignum) =
    let mutable __ret : bignum = Unchecked.defaultof<bignum>
    let mutable coef = coef
    let mutable f = f
    try
        if coef = 1 then
            __ret <- f
            raise Return
        if coef < 0 then
            __ret <- -(unbox<bignum> (tanEval (-coef) f))
            raise Return
        let ca: int = coef / 2
        let cb: int = coef - ca
        let a: bignum = tanEval ca f
        let b: bignum = tanEval cb f
        __ret <- (unbox<bignum> (a + b)) / (unbox<bignum> ((unbox<bignum> 1) - (a * b)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec tans (m: Map<string, int> array) =
    let mutable __ret : bignum = Unchecked.defaultof<bignum>
    let mutable m = m
    try
        if (unbox<int> (Array.length m)) = 1 then
            let t: Map<string, int> = m.[0]
            __ret <- tanEval (unbox<int> (t.["a"] |> unbox<int>)) (br (unbox<int> (t.["n"] |> unbox<int>)) (unbox<int> (t.["d"] |> unbox<int>)))
            raise Return
        let half: int = (unbox<int> (Array.length m)) / 2
        let a: bignum = tans (Array.sub m 0 (half - 0))
        let b: bignum = tans (Array.sub m half ((unbox<int> (Array.length m)) - half))
        __ret <- (unbox<bignum> (a + b)) / (unbox<bignum> ((unbox<bignum> 1) - (a * b)))
        raise Return
        __ret
    with
        | Return -> __ret
for ts in testCases do
    printfn "%s" ((("tan " + (format ts)) + " = ") + (string (tans ts)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
