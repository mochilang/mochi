// Generated 2025-08-11 15:32 +0700

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
let rec join (separator: string) (separated: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable separator = separator
    let mutable separated = separated
    try
        let mutable joined: string = ""
        let last_index: int = (Seq.length (separated)) - 1
        let mutable i: int = 0
        while i < (Seq.length (separated)) do
            joined <- joined + (_idx separated (int i))
            if i < last_index then
                joined <- joined + separator
            i <- i + 1
        __ret <- joined
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (join ("") (unbox<string array> [|"a"; "b"; "c"; "d"|]))
        printfn "%s" (join ("#") (unbox<string array> [|"a"; "b"; "c"; "d"|]))
        printfn "%s" (join ("#") (unbox<string array> [|"a"|]))
        printfn "%s" (join (" ") (unbox<string array> [|"You"; "are"; "amazing!"|]))
        printfn "%s" (join (",") (unbox<string array> [|""; ""; ""|]))
        printfn "%s" (join ("-") (unbox<string array> [|"apple"; "banana"; "cherry"|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
