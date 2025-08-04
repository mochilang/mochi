// Generated 2025-08-04 15:16 +0700

exception Return
let mutable __ret = ()

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec hailstone (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable seq: int array = [||]
        let mutable x: int = n
        seq <- Array.append seq [|x|]
        while x > 1 do
            if (((x % 2 + 2) % 2)) = 0 then
                x <- x / 2
            else
                x <- (3 * x) + 1
            seq <- Array.append seq [|x|]
        __ret <- seq
        raise Return
        __ret
    with
        | Return -> __ret
and listString (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (string (_idx xs i))
            if i < ((Seq.length (xs)) - 1) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and libMain () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable seq: int array = hailstone (27)
        printfn "%s" ("")
        printfn "%s" ("Hailstone sequence for the number 27:")
        printfn "%s" (("  has " + (string (Seq.length (seq)))) + " elements")
        printfn "%s" ("  starts with " + (listString (Array.sub seq 0 (4 - 0))))
        printfn "%s" ("  ends with " + (listString (Array.sub seq ((Seq.length (seq)) - 4) ((Seq.length (seq)) - ((Seq.length (seq)) - 4)))))
        let mutable longest: int = 0
        let mutable length: int = 0
        let mutable i: int = 1
        while i < 100000 do
            let l: int = Seq.length (hailstone (i))
            if l > length then
                longest <- i
                length <- l
            i <- i + 1
        printfn "%s" ("")
        printfn "%s" ((((string (longest)) + " has the longest Hailstone sequence, its length being ") + (string (length))) + ".")
        __ret
    with
        | Return -> __ret
libMain()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
