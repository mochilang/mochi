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
let rec padRight (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable r: string = s
        while (String.length r) < w do
            r <- r + " "
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and linearCombo (c: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (Seq.length c) do
            let n: int = c.[i]
            if n <> 0 then
                let mutable op: string = ""
                if (n < 0) && ((String.length out) = 0) then
                    op <- "-"
                else
                    if n < 0 then
                        op <- " - "
                    else
                        if (n > 0) && ((String.length out) = 0) then
                            op <- ""
                        else
                            op <- " + "
                let mutable av: int = n
                if av < 0 then
                    av <- -av
                let mutable coeff: string = (string av) + "*"
                if av = 1 then
                    coeff <- ""
                out <- ((((out + op) + coeff) + "e(") + (string (i + 1))) + ")"
            i <- i + 1
        if (String.length out) = 0 then
            __ret <- "0"
            raise Return
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
        let combos: int array array = [|[|1; 2; 3|]; [|0; 1; 2; 3|]; [|1; 0; 3; 4|]; [|1; 2; 0|]; [|0; 0; 0|]; [|0|]; [|1; 1; 1|]; [|-1; -1; -1|]; [|-1; -2; 0; -3|]; [|-1|]|]
        let mutable idx: int = 0
        while idx < (Seq.length combos) do
            let c: int array = combos.[idx]
            let mutable t: string = "["
            let mutable j: int = 0
            while j < (Seq.length c) do
                t <- t + (string (c.[j]))
                if j < ((Seq.length c) - 1) then
                    t <- t + ", "
                j <- j + 1
            t <- t + "]"
            let lc: string = linearCombo c
            printfn "%s" (((unbox<string> (padRight t 15)) + "  ->  ") + lc)
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
