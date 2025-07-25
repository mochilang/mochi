// Generated 2025-07-26 04:38 +0700

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
let rec mean (v: float array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable v = v
    try
        if (int (Array.length v)) = 0 then
            __ret <- Map.ofList [("ok", box false)]
            raise Return
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (int (Array.length v)) do
            sum <- sum + (float (v.[i]))
            i <- i + 1
        __ret <- Map.ofList [("ok", box true); ("mean", box (sum / (float (Array.length v))))]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let sets = [|[||]; [|3.0; 1.0; 4.0; 1.0; 5.0; 9.0|]; [|100000000000000000000.0; 3.0; 1.0; 4.0; 1.0; 5.0; 9.0; -100000000000000000000.0|]; [|10.0; 9.0; 8.0; 7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.11|]; [|10.0; 20.0; 30.0; 40.0; 50.0; -100.0; 4.7; -1100.0|]|]
        for v in sets do
            printfn "%s" ("Vector: " + (string v))
            let r: Map<string, obj> = mean (unbox<float array> v)
            if unbox<bool> (r.["ok"]) then
                printfn "%s" ((("Mean of " + (string (Seq.length v))) + " numbers is ") + (string (r.["mean"])))
            else
                printfn "%s" "Mean undefined"
            printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
