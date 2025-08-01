// Generated 2025-08-01 15:22 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let INF: int = 1000000000
        let n: int = 4
        let mutable dist: int array array = [||]
        let mutable next: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable nrow: int array = [||]
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|0|]
                else
                    row <- Array.append row [|INF|]
                nrow <- Array.append nrow [|0 - 1|]
                j <- j + 1
            dist <- Array.append dist [|row|]
            next <- Array.append next [|nrow|]
            i <- i + 1
        (dist.[0]).[2] <- -2
        (next.[0]).[2] <- 2
        (dist.[2]).[3] <- 2
        (next.[2]).[3] <- 3
        (dist.[3]).[1] <- -1
        (next.[3]).[1] <- 1
        (dist.[1]).[0] <- 4
        (next.[1]).[0] <- 0
        (dist.[1]).[2] <- 3
        (next.[1]).[2] <- 2
        let mutable k: int = 0
        while k < n do
            let mutable i: int = 0
            while i < n do
                let mutable j: int = 0
                while j < n do
                    if (((dist.[i]).[k]) < INF) && (((dist.[k]).[j]) < INF) then
                        let alt: int = ((dist.[i]).[k]) + ((dist.[k]).[j])
                        if alt < ((dist.[i]).[j]) then
                            (dist.[i]).[j] <- alt
                            (next.[i]).[j] <- (next.[i]).[k]
                    j <- j + 1
                i <- i + 1
            k <- k + 1
        let rec path (u: int) (v: int) =
            let mutable __ret : int array = Unchecked.defaultof<int array>
            let mutable u = u
            let mutable v = v
            try
                let mutable ui: int = u - 1
                let mutable vi: int = v - 1
                if ((next.[ui]).[vi]) = (0 - 1) then
                    __ret <- Array.empty<int>
                    raise Return
                let mutable p: int array = [|u|]
                let mutable cur: int = ui
                while cur <> vi do
                    cur <- (next.[cur]).[vi]
                    p <- Array.append p [|cur + 1|]
                __ret <- p
                raise Return
                __ret
            with
                | Return -> __ret
        let rec pathStr (p: int array) =
            let mutable __ret : string = Unchecked.defaultof<string>
            let mutable p = p
            try
                let mutable s: string = ""
                let mutable first: bool = true
                let mutable idx: int = 0
                while idx < (Seq.length p) do
                    let x: int = p.[idx]
                    if not first then
                        s <- s + " -> "
                    s <- s + (string x)
                    first <- false
                    idx <- idx + 1
                __ret <- s
                raise Return
                __ret
            with
                | Return -> __ret
        printfn "%s" "pair\tdist\tpath"
        let mutable a: int = 0
        while a < n do
            let mutable b: int = 0
            while b < n do
                if a <> b then
                    printfn "%s" (((((((string (a + 1)) + " -> ") + (string (b + 1))) + "\t") + (string ((dist.[a]).[b]))) + "\t") + (unbox<string> (pathStr (path (a + 1) (b + 1)))))
                b <- b + 1
            a <- a + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
