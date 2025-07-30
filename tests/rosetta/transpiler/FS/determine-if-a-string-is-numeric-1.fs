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
let rec isNumeric (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if s = "NaN" then
            __ret <- true
            raise Return
        let mutable i: int = 0
        if (String.length s) = 0 then
            __ret <- false
            raise Return
        if ((string (s.[0])) = "+") || ((string (s.[0])) = "-") then
            if (String.length s) = 1 then
                __ret <- false
                raise Return
            i <- 1
        let mutable digits: bool = false
        let mutable dot: bool = false
        while i < (String.length s) do
            let ch: string = string (s.[i])
            if (ch >= "0") && (ch <= "9") then
                digits <- true
                i <- i + 1
            else
                if (ch = ".") && (dot = false) then
                    dot <- true
                    i <- i + 1
                else
                    if ((ch = "e") || (ch = "E")) && digits then
                        i <- i + 1
                        if (i < (String.length s)) && (((string (s.[i])) = "+") || ((string (s.[i])) = "-")) then
                            i <- i + 1
                        let mutable ed: bool = false
                        while ((i < (String.length s)) && ((string (s.[i])) >= "0")) && ((string (s.[i])) <= "9") do
                            ed <- true
                            i <- i + 1
                        __ret <- ed && (i = (String.length s))
                        raise Return
                    else
                        __ret <- false
                        raise Return
        __ret <- digits
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "Are these strings numeric?"
        let strs: string array = [|"1"; "3.14"; "-100"; "1e2"; "NaN"; "rose"|]
        for s in strs do
            printfn "%s" ((("  " + s) + " -> ") + (string (isNumeric s)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
