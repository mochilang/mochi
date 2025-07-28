// Generated 2025-07-28 10:03 +0700

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
type Anon1 = {
    pm: int
    g1: int
    s1: int
    g2: int
    s2: int
    d: int
}
let rec commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable i: int = (String.length s) - 3
        while i > 0 do
            s <- ((s.Substring(0, i - 0)) + ",") + (s.Substring(i, (String.length s) - i))
            i <- i - 3
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: Anon1 array = [|box (Map.ofList [("pm", 10); ("g1", 4); ("s1", 7); ("g2", 6); ("s2", 23); ("d", 16)]); box (Map.ofList [("pm", 100); ("g1", 14); ("s1", 113); ("g2", 16); ("s2", 1831); ("d", 1718)]); box (Map.ofList [("pm", 1000); ("g1", 14); ("s1", 113); ("g2", 16); ("s2", 1831); ("d", 1718)]); box (Map.ofList [("pm", 10000); ("g1", 36); ("s1", 9551); ("g2", 38); ("s2", 30593); ("d", 21042)]); box (Map.ofList [("pm", 100000); ("g1", 70); ("s1", 173359); ("g2", 72); ("s2", 31397); ("d", 141962)]); box (Map.ofList [("pm", 1000000); ("g1", 100); ("s1", 396733); ("g2", 102); ("s2", 1444309); ("d", 1047576)]); box (Map.ofList [("pm", 10000000); ("g1", 148); ("s1", 2010733); ("g2", 150); ("s2", 13626257); ("d", 11615524)]); box (Map.ofList [("pm", 100000000); ("g1", 198); ("s1", 46006769); ("g2", 200); ("s2", 378043979); ("d", 332037210)]); box (Map.ofList [("pm", 1000000000); ("g1", 276); ("s1", 649580171); ("g2", 278); ("s2", -34038695); ("d", -683618866)]); box (Map.ofList [("pm", box 5705032704); ("g1", box 332); ("s1", box 1598212825); ("g2", box 334); ("s2", box 26532171213); ("d", box 20638991092)]); box (Map.ofList [("pm", box 95705032704); ("g1", box 386); ("s1", box 30943678291); ("g2", box 388); ("s2", box 152503824927); ("d", box 117265179340)])|]
        for entry in data do
            let pm: string = commatize (int (entry.["pm"]))
            let line1: string = ("Earliest difference > " + pm) + " between adjacent prime gap starting primes:"
            printfn "%s" line1
            let line2: string = ((((((((("Gap " + (string (entry.["g1"]))) + " starts at ") + (unbox<string> (commatize (int (entry.["s1"]))))) + ", gap ") + (string (entry.["g2"]))) + " starts at ") + (unbox<string> (commatize (int (entry.["s2"]))))) + ", difference is ") + (unbox<string> (commatize (int (entry.["d"]))))) + "."
            printfn "%s" line2
            printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
