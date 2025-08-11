// Generated 2025-08-11 17:23 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec alternative_string_arrange (first_str: string) (second_str: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable first_str = first_str
    let mutable second_str = second_str
    try
        let len1: int = String.length (first_str)
        let len2: int = String.length (second_str)
        let mutable res: string = ""
        let mutable i: int = 0
        while (i < len1) || (i < len2) do
            if i < len1 then
                res <- res + (string (first_str.[i]))
            if i < len2 then
                res <- res + (string (second_str.[i]))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (alternative_string_arrange ("ABCD") ("XY"))
printfn "%s" (alternative_string_arrange ("XY") ("ABCD"))
printfn "%s" (alternative_string_arrange ("AB") ("XYZ"))
printfn "%s" (alternative_string_arrange ("ABC") (""))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
