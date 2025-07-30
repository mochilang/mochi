// Generated 2025-07-31 00:10 +0700

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
type DotResult = {
    value: int
    ok: bool
}
let rec dot (x: int array) (y: int array) =
    let mutable __ret : DotResult = Unchecked.defaultof<DotResult>
    let mutable x = x
    let mutable y = y
    try
        if (Seq.length x) <> (Seq.length y) then
            __ret <- { value = 0; ok = false }
            raise Return
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < (Seq.length x) do
            sum <- sum + ((x.[i]) * (y.[i]))
            i <- i + 1
        __ret <- { value = sum; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let r: DotResult = dot [|1; 3; -5|] [|4; -2; -1|]
        if not (r.ok) then
            printfn "%s" "incompatible lengths"
        else
            printfn "%s" (string (r.value))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
