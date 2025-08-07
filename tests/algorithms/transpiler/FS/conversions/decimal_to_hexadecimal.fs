// Generated 2025-08-07 10:31 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let values: string array = [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "a"; "b"; "c"; "d"; "e"; "f"|]
let rec decimal_to_hexadecimal (decimal: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable decimal = decimal
    try
        let mutable num: int = decimal
        let mutable negative: bool = false
        if num < 0 then
            negative <- true
            num <- -num
        if num = 0 then
            if negative then
                __ret <- "-0x0"
                raise Return
            __ret <- "0x0"
            raise Return
        let mutable hex: string = ""
        while num > 0 do
            let remainder: int = ((num % 16 + 16) % 16)
            hex <- (_idx values (remainder)) + hex
            num <- num / 16
        if negative then
            __ret <- "-0x" + hex
            raise Return
        __ret <- "0x" + hex
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (decimal_to_hexadecimal (5))
printfn "%s" (decimal_to_hexadecimal (15))
printfn "%s" (decimal_to_hexadecimal (37))
printfn "%s" (decimal_to_hexadecimal (255))
printfn "%s" (decimal_to_hexadecimal (4096))
printfn "%s" (decimal_to_hexadecimal (999098))
printfn "%s" (decimal_to_hexadecimal (-256))
printfn "%s" (decimal_to_hexadecimal (0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
