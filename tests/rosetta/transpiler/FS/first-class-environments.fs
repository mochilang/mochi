// Generated 2025-07-30 21:05 +0700

exception Break
exception Continue

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
type HailResult = {
    seq: int
    cnt: int
    out: string
}
let jobs: int = 12
let rec pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        while (String.length s) < 4 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and hail (seq: int) (cnt: int) =
    let mutable __ret : HailResult = Unchecked.defaultof<HailResult>
    let mutable seq = seq
    let mutable cnt = cnt
    try
        let mutable out: string = pad seq
        if seq <> 1 then
            cnt <- cnt + 1
            if (((seq % 2 + 2) % 2)) <> 0 then
                seq <- (3 * seq) + 1
            else
                seq <- seq / 2
        __ret <- { seq = seq; cnt = cnt; out = out }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable seqs: int array = [||]
        let mutable cnts: int array = [||]
        for i in 0 .. (jobs - 1) do
            seqs <- Array.append seqs [|i + 1|]
            cnts <- Array.append cnts [|0|]
        try
            while true do
                try
                    let mutable line: string = ""
                    let mutable i: int = 0
                    while i < jobs do
                        let res: HailResult = hail (seqs.[i]) (cnts.[i])
                        seqs.[i] <- res.seq
                        cnts.[i] <- res.cnt
                        line <- line + (res.out)
                        i <- i + 1
                    printfn "%s" line
                    let mutable ``done``: bool = true
                    let mutable j: int = 0
                    while j < jobs do
                        if (seqs.[j]) <> 1 then
                            ``done`` <- false
                        j <- j + 1
                    if ``done`` then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" ""
        printfn "%s" "COUNTS:"
        let mutable counts: string = ""
        let mutable k: int = 0
        while k < jobs do
            counts <- counts + (unbox<string> (pad (cnts.[k])))
            k <- k + 1
        printfn "%s" counts
        printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
