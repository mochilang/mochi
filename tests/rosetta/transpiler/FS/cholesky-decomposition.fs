// Generated 2025-07-27 23:45 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec cholesky (a: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    try
        let n: int = Array.length a
        let mutable l: float array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < n do
                row <- unbox<float array> (Array.append row [|0.0|])
                j <- j + 1
            l <- unbox<float array array> (Array.append l [|row|])
            i <- i + 1
        i <- 0
        while i < n do
            let mutable j: int = 0
            while j <= i do
                let mutable sum = (a.[i]).[j]
                let mutable k: int = 0
                while k < j do
                    sum <- sum - (((l.[i]).[k]) * ((l.[j]).[k]))
                    k <- k + 1
                if i = j then
                    (l.[i]).[j] <- sqrtApprox (unbox<float> sum)
                else
                    (l.[i]).[j] <- sum / ((l.[j]).[j])
                j <- j + 1
            i <- i + 1
        __ret <- unbox<float array array> l
        raise Return
        __ret
    with
        | Return -> __ret
let rec printMat (m: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        let mutable i: int = 0
        while i < (unbox<int> (Array.length m)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (m.[i])) do
                line <- line + (string ((m.[i]).[j]))
                if j < ((Seq.length (m.[i])) - 1) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec demo (a: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    try
        printfn "%s" "A:"
        printMat a
        let l: float array array = cholesky a
        printfn "%s" "L:"
        printMat l
        __ret
    with
        | Return -> __ret
demo [|[|25.0; 15.0; -5.0|]; [|15.0; 18.0; 0.0|]; [|-5.0; 0.0; 11.0|]|]
demo [|[|18.0; 22.0; 54.0; 42.0|]; [|22.0; 70.0; 86.0; 62.0|]; [|54.0; 86.0; 174.0; 134.0|]; [|42.0; 62.0; 134.0; 106.0|]|]
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
