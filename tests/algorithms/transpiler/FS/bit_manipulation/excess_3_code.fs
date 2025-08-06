// Generated 2025-08-06 21:33 +0700

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
let rec excess_3_code (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        let mutable n: int = number
        if n < 0 then
            n <- 0
        let mapping: string array = [|"0011"; "0100"; "0101"; "0110"; "0111"; "1000"; "1001"; "1010"; "1011"; "1100"|]
        let mutable res: string = ""
        if n = 0 then
            res <- _idx mapping (0)
        else
            while n > 0 do
                let digit: int = ((n % 10 + 10) % 10)
                res <- (_idx mapping (digit)) + res
                n <- n / 10
        __ret <- "0b" + res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (excess_3_code (0))
        printfn "%s" (excess_3_code (3))
        printfn "%s" (excess_3_code (2))
        printfn "%s" (excess_3_code (20))
        printfn "%s" (excess_3_code (120))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
