// Generated 2025-08-04 20:03 +0700

exception Return
let mutable __ret = ()

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec p (x: float) (e: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable e = e
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < (int e) do
            r <- r * x
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and padInt (f: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        let s: string = string (int f)
        if f >= (float 0) then
            __ret <- " " + s
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable ops: string array = [|"-x.p(e)"; "-(x).p(e)"; "(-x).p(e)"; "-(x.p(e))"|]
for x in [|-5.0; 5.0|] do
    for e in [|2.0; 3.0|] do
        let a: float = -(p (float x) (float e))
        let b: float = -(p (float x) (float e))
        let c: float = p (float (-x)) (float e)
        let d: float = -(p (float x) (float e))
        printfn "%s" (((((((((((((((((((("x = " + (if (int x) < 0 then "" else " ")) + (string (int x))) + " e = ") + (string (int e))) + " | ") + (_idx ops 0)) + " = ") + (padInt (a))) + " | ") + (_idx ops 1)) + " = ") + (padInt (b))) + " | ") + (_idx ops 2)) + " = ") + (padInt (c))) + " | ") + (_idx ops 3)) + " = ") + (padInt (d)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
