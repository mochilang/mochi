// Generated 2025-07-28 10:03 +0700

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
        let philosophers: string array = [|"Aristotle"; "Kant"; "Spinoza"; "Marx"; "Russell"|]
        let hunger: int = 3
        printfn "%s" "table empty"
        for p in philosophers do
            printfn "%s" (p + " seated")
        let mutable idx: int = 0
        while idx < (Seq.length philosophers) do
            let name: string = philosophers.[idx]
            let mutable h: int = 0
            while h < hunger do
                printfn "%s" (name + " hungry")
                printfn "%s" (name + " eating")
                printfn "%s" (name + " thinking")
                h <- h + 1
            printfn "%s" (name + " satisfied")
            printfn "%s" (name + " left the table")
            idx <- idx + 1
        printfn "%s" "table empty"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
