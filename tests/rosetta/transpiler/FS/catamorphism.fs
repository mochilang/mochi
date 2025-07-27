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
let rec add (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a + b
        raise Return
        __ret
    with
        | Return -> __ret
let rec sub (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a - b
        raise Return
        __ret
    with
        | Return -> __ret
let rec mul (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a * b
        raise Return
        __ret
    with
        | Return -> __ret
let rec fold (f: int -> int -> int) (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable xs = xs
    try
        let mutable r: int = xs.[0]
        let mutable i: int = 1
        while i < (int (Array.length xs)) do
            r <- int (f r (xs.[i]))
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let n: int array = [|1; 2; 3; 4; 5|]
printfn "%d" (fold (unbox<int -> int -> int> (fun (a: int) (b: int) -> (add a b))) n)
printfn "%d" (fold (unbox<int -> int -> int> (fun (a: int) (b: int) -> (sub a b))) n)
printfn "%d" (fold (unbox<int -> int -> int> (fun (a: int) (b: int) -> (mul a b))) n)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
