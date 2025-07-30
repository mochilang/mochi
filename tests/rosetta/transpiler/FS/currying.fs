// Generated 2025-07-30 21:41 +0700

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
type Foo = {
    value: int
}
let rec Foo_Method (self: Foo) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable self = self
    let mutable b = b
    try
        let mutable value: int = self.value
        __ret <- value + b
        raise Return
        __ret
    with
        | Return -> __ret
and pow (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < (int (int exp)) do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and PowN (b: float) =
    let mutable __ret : float -> float = Unchecked.defaultof<float -> float>
    let mutable b = b
    try
        __ret <- unbox<float -> float> (        fun (e: float) -> (pow b e))
        raise Return
        __ret
    with
        | Return -> __ret
and PowE (e: float) =
    let mutable __ret : float -> float = Unchecked.defaultof<float -> float>
    let mutable e = e
    try
        __ret <- unbox<float -> float> (        fun (b: float) -> (pow b e))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let pow2: float -> float = PowN 2.0
        let cube: float -> float = PowE 3.0
        printfn "%s" ("2^8 = " + (string (pow2 8.0)))
        printfn "%s" ("4³ = " + (string (cube 4.0)))
        let mutable a: Foo = { value = 2 }
        let fn1: int -> int =         fun (b: int) -> (Foo_Method a b)
        let fn2: Foo -> int -> int =         fun (f: Foo) (b: int) -> (Foo_Method f b)
        printfn "%s" ("2 + 2 = " + (string (Foo_Method a 2)))
        printfn "%s" ("2 + 3 = " + (string (fn1 3)))
        printfn "%s" ("2 + 4 = " + (string (fn2 a 4)))
        printfn "%s" ("3 + 5 = " + (string (fn2 { value = 3 } 5)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
