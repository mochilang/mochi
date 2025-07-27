// Generated 2025-07-27 23:36 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec br (n: int) (d: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    let mutable d = d
    try
        __ret <- (float n) / (float (float d))
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testCases: Map<string, int> array array = [|[|Map.ofList [("a", 1); ("n", 1); ("d", 2)]; Map.ofList [("a", 1); ("n", 1); ("d", 3)]|]; [|Map.ofList [("a", 2); ("n", 1); ("d", 3)]; Map.ofList [("a", 1); ("n", 1); ("d", 7)]|]; [|Map.ofList [("a", 4); ("n", 1); ("d", 5)]; Map.ofList [("a", -1); ("n", 1); ("d", 239)]|]; [|Map.ofList [("a", 5); ("n", 1); ("d", 7)]; Map.ofList [("a", 2); ("n", 3); ("d", 79)]|]; [|Map.ofList [("a", 1); ("n", 1); ("d", 2)]; Map.ofList [("a", 1); ("n", 1); ("d", 5)]; Map.ofList [("a", 1); ("n", 1); ("d", 8)]|]; [|Map.ofList [("a", 4); ("n", 1); ("d", 5)]; Map.ofList [("a", -1); ("n", 1); ("d", 70)]; Map.ofList [("a", 1); ("n", 1); ("d", 99)]|]; [|Map.ofList [("a", 5); ("n", 1); ("d", 7)]; Map.ofList [("a", 4); ("n", 1); ("d", 53)]; Map.ofList [("a", 2); ("n", 1); ("d", 4443)]|]; [|Map.ofList [("a", 6); ("n", 1); ("d", 8)]; Map.ofList [("a", 2); ("n", 1); ("d", 57)]; Map.ofList [("a", 1); ("n", 1); ("d", 239)]|]; [|Map.ofList [("a", 8); ("n", 1); ("d", 10)]; Map.ofList [("a", -1); ("n", 1); ("d", 239)]; Map.ofList [("a", -4); ("n", 1); ("d", 515)]|]; [|Map.ofList [("a", 12); ("n", 1); ("d", 18)]; Map.ofList [("a", 8); ("n", 1); ("d", 57)]; Map.ofList [("a", -5); ("n", 1); ("d", 239)]|]; [|Map.ofList [("a", 16); ("n", 1); ("d", 21)]; Map.ofList [("a", 3); ("n", 1); ("d", 239)]; Map.ofList [("a", 4); ("n", 3); ("d", 1042)]|]; [|Map.ofList [("a", 22); ("n", 1); ("d", 28)]; Map.ofList [("a", 2); ("n", 1); ("d", 443)]; Map.ofList [("a", -5); ("n", 1); ("d", 1393)]; Map.ofList [("a", -10); ("n", 1); ("d", 11018)]|]; [|Map.ofList [("a", 22); ("n", 1); ("d", 38)]; Map.ofList [("a", 17); ("n", 7); ("d", 601)]; Map.ofList [("a", 10); ("n", 7); ("d", 8149)]|]; [|Map.ofList [("a", 44); ("n", 1); ("d", 57)]; Map.ofList [("a", 7); ("n", 1); ("d", 239)]; Map.ofList [("a", -12); ("n", 1); ("d", 682)]; Map.ofList [("a", 24); ("n", 1); ("d", 12943)]|]; [|Map.ofList [("a", 88); ("n", 1); ("d", 172)]; Map.ofList [("a", 51); ("n", 1); ("d", 239)]; Map.ofList [("a", 32); ("n", 1); ("d", 682)]; Map.ofList [("a", 44); ("n", 1); ("d", 5357)]; Map.ofList [("a", 68); ("n", 1); ("d", 12943)]|]; [|Map.ofList [("a", 88); ("n", 1); ("d", 172)]; Map.ofList [("a", 51); ("n", 1); ("d", 239)]; Map.ofList [("a", 32); ("n", 1); ("d", 682)]; Map.ofList [("a", 44); ("n", 1); ("d", 5357)]; Map.ofList [("a", 68); ("n", 1); ("d", 12944)]|]|]
let rec format (ts: Map<string, int> array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ts = ts
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (int (Array.length ts)) do
            let t: Map<string, int> = ts.[i]
            s <- ((((((s + "{") + (string (t.["a"] |> unbox<int>))) + " ") + (string (t.["n"] |> unbox<int>))) + " ") + (string (t.["d"] |> unbox<int>))) + "}"
            if i < (int ((int (Array.length ts)) - 1)) then
                s <- s + " "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec tanEval (coef: int) (f: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable coef = coef
    let mutable f = f
    try
        if coef = 1 then
            __ret <- f
            raise Return
        if coef < 0 then
            __ret <- -(float (tanEval (-coef) f))
            raise Return
        let ca: int = coef / 2
        let cb: int = coef - ca
        let a: float = tanEval ca f
        let b: float = tanEval cb f
        __ret <- (float (a + b)) / (float ((float 1) - (a * b)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec tans (m: Map<string, int> array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        if (int (Array.length m)) = 1 then
            let t: Map<string, int> = m.[0]
            __ret <- tanEval (int (t.["a"] |> unbox<int>)) (br (int (t.["n"] |> unbox<int>)) (int (t.["d"] |> unbox<int>)))
            raise Return
        let half: int = (int (Array.length m)) / 2
        let a: float = tans (Array.sub m 0 (half - 0))
        let b: float = tans (Array.sub m half ((int (Array.length m)) - half))
        __ret <- (float (a + b)) / (float ((float 1) - (a * b)))
        raise Return
        __ret
    with
        | Return -> __ret
for ts in testCases do
    printfn "%s" ((("tan " + (unbox<string> (format ts))) + " = ") + (string (tans ts)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
