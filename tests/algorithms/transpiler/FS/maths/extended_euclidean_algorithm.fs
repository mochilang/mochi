// Generated 2025-08-12 08:17 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type Coeffs = {
    mutable _x: int
    mutable _y: int
}
let rec abs_val (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and extended_euclidean_algorithm (a: int) (b: int) =
    let mutable __ret : Coeffs = Unchecked.defaultof<Coeffs>
    let mutable a = a
    let mutable b = b
    try
        if (abs_val (a)) = 1 then
            __ret <- { _x = a; _y = 0 }
            raise Return
        if (abs_val (b)) = 1 then
            __ret <- { _x = 0; _y = b }
            raise Return
        let mutable old_remainder: int = a
        let mutable remainder: int = b
        let mutable old_coeff_a: int = 1
        let mutable coeff_a: int = 0
        let mutable old_coeff_b: int = 0
        let mutable coeff_b: int = 1
        while remainder <> 0 do
            let quotient: int = _floordiv old_remainder remainder
            let temp_remainder: int = old_remainder - (quotient * remainder)
            old_remainder <- remainder
            remainder <- temp_remainder
            let temp_a: int = old_coeff_a - (quotient * coeff_a)
            old_coeff_a <- coeff_a
            coeff_a <- temp_a
            let temp_b: int = old_coeff_b - (quotient * coeff_b)
            old_coeff_b <- coeff_b
            coeff_b <- temp_b
        if a < 0 then
            old_coeff_a <- -old_coeff_a
        if b < 0 then
            old_coeff_b <- -old_coeff_b
        __ret <- { _x = old_coeff_a; _y = old_coeff_b }
        raise Return
        __ret
    with
        | Return -> __ret
and test_extended_euclidean_algorithm () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let r1: Coeffs = extended_euclidean_algorithm (1) (24)
        if ((r1._x) <> 1) || ((r1._y) <> 0) then
            failwith ("test1 failed")
        let r2: Coeffs = extended_euclidean_algorithm (8) (14)
        if ((r2._x) <> 2) || ((r2._y) <> (-1)) then
            failwith ("test2 failed")
        let r3: Coeffs = extended_euclidean_algorithm (240) (46)
        if ((r3._x) <> (-9)) || ((r3._y) <> 47) then
            failwith ("test3 failed")
        let r4: Coeffs = extended_euclidean_algorithm (1) (-4)
        if ((r4._x) <> 1) || ((r4._y) <> 0) then
            failwith ("test4 failed")
        let r5: Coeffs = extended_euclidean_algorithm (-2) (-4)
        if ((r5._x) <> (-1)) || ((r5._y) <> 0) then
            failwith ("test5 failed")
        let r6: Coeffs = extended_euclidean_algorithm (0) (-4)
        if ((r6._x) <> 0) || ((r6._y) <> (-1)) then
            failwith ("test6 failed")
        let r7: Coeffs = extended_euclidean_algorithm (2) (0)
        if ((r7._x) <> 1) || ((r7._y) <> 0) then
            failwith ("test7 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_extended_euclidean_algorithm()
        let res: Coeffs = extended_euclidean_algorithm (240) (46)
        printfn "%s" (((("(" + (_str (res._x))) + ", ") + (_str (res._y))) + ")")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
