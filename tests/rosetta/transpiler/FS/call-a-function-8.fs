// Generated 2025-07-27 10:08 +0000

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
let rec mapString (s: string) (f: string -> string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable f = f
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            out <- out + (unbox<string> (f (s.Substring(i, (i + 1) - i))))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let fn: string -> string =         fun (r: string) -> (if r = " " then "" else r)
        mapString "Spaces removed" fn
        mapString "Test" (unbox<string -> string> (        fun (r: string) -> (r.ToLower())))
        mapString "shift" (unbox<string -> string> (        fun (r: string) -> r))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
