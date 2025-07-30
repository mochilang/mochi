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
type cds = {
    i: int
    s: string
    b: int array
    m: Map<int, bool>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec copyList (src: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable src = src
    try
        let mutable out: int array = [||]
        for v in src do
            out <- Array.append out [|v|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec copyMap (src: Map<int, bool>) =
    let mutable __ret : Map<int, bool> = Unchecked.defaultof<Map<int, bool>>
    let mutable src = src
    try
        let mutable out: Map<int, bool> = Map.ofList []
        for KeyValue(k, _) in src do
            out <- Map.add k (src.[k] |> unbox<bool>) out
        __ret <- unbox<Map<int, bool>> out
        raise Return
        __ret
    with
        | Return -> __ret
let rec deepcopy (c: cds) =
    let mutable __ret : cds = Unchecked.defaultof<cds>
    let mutable c = c
    try
        __ret <- { i = c.i; s = c.s; b = copyList (c.b); m = copyMap (c.m) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec cdsStr (c: cds) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable bs: string = "["
        let mutable i: int = 0
        while i < (Seq.length (c.b)) do
            bs <- bs + (string ((c.b).[i]))
            if i < ((Seq.length (c.b)) - 1) then
                bs <- bs + " "
            i <- i + 1
        bs <- bs + "]"
        let mutable ms: string = "map["
        let mutable first: bool = true
        for k in c.m do
            if not first then
                ms <- ms + " "
            ms <- ((ms + (string k)) + ":") + (string (c.m.[k] |> unbox<bool>))
            first <- false
        ms <- ms + "]"
        __ret <- ((((((("{" + (string (c.i))) + " ") + (c.s)) + " ") + bs) + " ") + ms) + "}"
        raise Return
        __ret
    with
        | Return -> __ret
let mutable c1: cds = { i = 1; s = "one"; b = [|117; 110; 105; 116|]; m = Map.ofList [(1, box true)] }
let mutable c2: cds = deepcopy c1
printfn "%s" (cdsStr c1)
printfn "%s" (cdsStr c2)
c1 <- { i = 0; s = "nil"; b = [|122; 101; 114; 111|]; m = Map.ofList [(1, box false)] }
printfn "%s" (cdsStr c1)
printfn "%s" (cdsStr c2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
