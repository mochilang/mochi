// Generated 2025-07-30 21:41 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable rows: int array array = [||]
        for i in 0 .. (4 - 1) do
            rows <- Array.append rows [|[|i * 3; (i * 3) + 1; (i * 3) + 2|]|]
        printfn "%s" "<table>"
        printfn "%s" "    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>"
        let mutable idx: int = 0
        for row in rows do
            printfn "%s" (((((((("    <tr><td>" + (string idx)) + "</td><td>") + (string (row.[0]))) + "</td><td>") + (string (row.[1]))) + "</td><td>") + (string (row.[2]))) + "</td></tr>")
            idx <- idx + 1
        printfn "%s" "</table>"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
