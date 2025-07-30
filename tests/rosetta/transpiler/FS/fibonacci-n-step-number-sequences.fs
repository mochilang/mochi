// Generated 2025-07-30 21:05 +0700

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
let rec show (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                s <- s + " "
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and gen (init: int array) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable init = init
    let mutable n = n
    try
        let mutable b: int array = init
        let mutable res: int array = [||]
        let mutable sum: int = 0
        for x in b do
            res <- Array.append res [|x|]
            sum <- sum + x
        while (Seq.length res) < n do
            let mutable next: int = sum
            res <- Array.append res [|next|]
            sum <- (sum + next) - (b.[0])
            b <- Array.append (Array.sub b 1 ((Seq.length b) - 1)) [|next|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let n: int = 10
        printfn "%s" (" Fibonacci: " + (unbox<string> (show (gen [|1; 1|] n))))
        printfn "%s" ("Tribonacci: " + (unbox<string> (show (gen [|1; 1; 2|] n))))
        printfn "%s" ("Tetranacci: " + (unbox<string> (show (gen [|1; 1; 2; 4|] n))))
        printfn "%s" ("     Lucas: " + (unbox<string> (show (gen [|2; 1|] n))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
