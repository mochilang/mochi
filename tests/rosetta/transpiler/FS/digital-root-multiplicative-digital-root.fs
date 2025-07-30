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
type MDRResult = {
    mp: int
    mdr: int
}
let rec pad (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and mult (n: bigint) (``base``: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let mutable m: bigint = bigint 1
        let mutable x: bigint = n
        let b: bigint = bigint ``base``
        while x > (bigint 0) do
            m <- m * (((x % b + b) % b))
            x <- x / b
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and multDigitalRoot (n: bigint) (``base``: int) =
    let mutable __ret : MDRResult = Unchecked.defaultof<MDRResult>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let mutable m: bigint = n
        let mutable mp: int = 0
        let b: bigint = bigint ``base``
        while m >= b do
            m <- mult m ``base``
            mp <- mp + 1
        __ret <- { mp = mp; mdr = int m }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ``base``: int = 10
        let size: int = 5
        printfn "%s" (((((unbox<string> (pad "Number" 20)) + " ") + (unbox<string> (pad "MDR" 3))) + " ") + (unbox<string> (pad "MP" 3)))
        let nums: bigint array = [|bigint 123321; bigint 7739; bigint 893; bigint 899998; bigint (int 3778888999L); bigint 277777788888899L|]
        let mutable i: int = 0
        while i < (Seq.length nums) do
            let mutable n: bigint = nums.[i]
            let r: MDRResult = multDigitalRoot n ``base``
            printfn "%s" (((((unbox<string> (pad (string n) 20)) + " ") + (unbox<string> (pad (string (r.mdr)) 3))) + " ") + (unbox<string> (pad (string (r.mp)) 3)))
            i <- i + 1
        printfn "%s" ""
        let mutable list: int array array = [||]
        let mutable idx: int = 0
        while idx < ``base`` do
            list <- Array.append list [|[||]|]
            idx <- idx + 1
        let mutable cnt: int = size * ``base``
        let mutable n: bigint = bigint 0
        let b: bigint = bigint ``base``
        while cnt > 0 do
            let r: MDRResult = multDigitalRoot n ``base``
            let mdr: int = r.mdr
            if (Seq.length (list.[mdr])) < size then
                list.[mdr] <- Array.append (list.[mdr]) [|int n|]
                cnt <- cnt - 1
            n <- n + (bigint 1)
        printfn "%s" "MDR: First"
        let mutable j: int = 0
        while j < ``base`` do
            printfn "%s" (((unbox<string> (pad (string j) 3)) + ": ") + (string (list.[j])))
            j <- j + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
