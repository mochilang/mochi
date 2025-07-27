// Generated 2025-07-27 23:36 +0700

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
let rec makeSym (order: int) (elements: float array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable order = order
    let mutable elements = elements
    try
        __ret <- unbox<Map<string, obj>> (Map.ofList [("order", box order); ("ele", box elements)])
        raise Return
        __ret
    with
        | Return -> __ret
let rec unpackSym (m: Map<string, obj>) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let n: obj = box (m.["order"])
        let ele: obj = box (m.["ele"])
        let mutable mat: float array array = [||]
        let mutable idx: int = 0
        let mutable r: int = 0
        while r < (unbox<int> n) do
            let mutable row: float array = [||]
            let mutable c: int = 0
            while c <= r do
                row <- unbox<float array> (Array.append row [|((ele :?> obj[]).[idx])|])
                idx <- idx + 1
                c <- c + 1
            while c < (unbox<int> n) do
                row <- unbox<float array> (Array.append row [|0.0|])
                c <- c + 1
            mat <- unbox<float array array> (Array.append mat [|row|])
            r <- r + 1
        r <- 0
        while r < (unbox<int> n) do
            let mutable c: int = r + 1
            while c < (unbox<int> n) do
                (mat.[r]).[c] <- (mat.[c]).[r]
                c <- c + 1
            r <- r + 1
        __ret <- unbox<float array array> mat
        raise Return
        __ret
    with
        | Return -> __ret
let rec printMat (m: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        let mutable i: int = 0
        while i < (int (Array.length m)) do
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
let rec printSym (m: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        printMat (unpackSym m)
        __ret
    with
        | Return -> __ret
let rec printLower (m: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        let n: obj = box (m.["order"])
        let ele: obj = box (m.["ele"])
        let mutable mat: float array array = [||]
        let mutable idx: int = 0
        let mutable r: int = 0
        while r < (unbox<int> n) do
            let mutable row: float array = [||]
            let mutable c: int = 0
            while c <= r do
                row <- unbox<float array> (Array.append row [|((ele :?> obj[]).[idx])|])
                idx <- idx + 1
                c <- c + 1
            while c < (unbox<int> n) do
                row <- unbox<float array> (Array.append row [|0.0|])
                c <- c + 1
            mat <- unbox<float array array> (Array.append mat [|row|])
            r <- r + 1
        printMat mat
        __ret
    with
        | Return -> __ret
let rec choleskyLower (a: Map<string, obj>) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable a = a
    try
        let n: obj = box (a.["order"])
        let ae: obj = box (a.["ele"])
        let mutable le: float array = [||]
        let mutable idx: int = 0
        while idx < (Seq.length ae) do
            le <- unbox<float array> (Array.append le [|0.0|])
            idx <- idx + 1
        let mutable row: int = 1
        let mutable col: int = 1
        let mutable dr: int = 0
        let mutable dc: int = 0
        let mutable i: int = 0
        while i < (Seq.length ae) do
            let e: obj = box (((ae :?> obj[]).[i]))
            if i < dr then
                let mutable d = (e - (le.[i])) / (le.[dc])
                le.[i] <- d
                let mutable ci: int = col
                let mutable cx: int = dc
                let mutable j: int = i + 1
                while j <= dr do
                    cx <- cx + ci
                    ci <- ci + 1
                    le.[j] <- (le.[j]) + (d * (le.[cx]))
                    j <- j + 1
                col <- col + 1
                dc <- dc + col
            else
                le.[i] <- sqrtApprox (float (e - (le.[i])))
                row <- row + 1
                dr <- dr + row
                col <- 1
                dc <- 0
            i <- i + 1
        __ret <- unbox<Map<string, obj>> (Map.ofList [("order", box n); ("ele", box le)])
        raise Return
        __ret
    with
        | Return -> __ret
let rec demo (a: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    try
        printfn "%s" "A:"
        printSym a
        printfn "%s" "L:"
        let l: Map<string, obj> = choleskyLower a
        printLower l
        __ret
    with
        | Return -> __ret
demo (makeSym 3 [|25.0; 15.0; 18.0; -5.0; 0.0; 11.0|])
demo (makeSym 4 [|18.0; 22.0; 70.0; 54.0; 86.0; 174.0; 42.0; 62.0; 134.0; 106.0|])
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
