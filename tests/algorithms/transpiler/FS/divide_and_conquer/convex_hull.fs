// Generated 2025-08-07 15:46 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
type Point = {
    x: int
    y: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec cross (o: Point) (a: Point) (b: Point) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable o = o
    let mutable a = a
    let mutable b = b
    try
        __ret <- (((a.x) - (o.x)) * ((b.y) - (o.y))) - (((a.y) - (o.y)) * ((b.x) - (o.x)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sortPoints (ps: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable ps = ps
    try
        let mutable arr: Point array = ps
        let mutable n: int = Seq.length (arr)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                let p: Point = _idx arr (j)
                let q: Point = _idx arr (j + 1)
                if ((p.x) > (q.x)) || (((p.x) = (q.x)) && ((p.y) > (q.y))) then
                    arr.[j] <- q
                    arr.[j + 1] <- p
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec convex_hull (ps: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable ps = ps
    try
        ps <- sortPoints (ps)
        let mutable lower: Point array = [||]
        for p in ps do
            while ((Seq.length (lower)) >= 2) && ((cross (_idx lower ((Seq.length (lower)) - 2)) (_idx lower ((Seq.length (lower)) - 1)) (p)) <= 0) do
                lower <- Array.sub lower 0 (((Seq.length (lower)) - 1) - 0)
            lower <- Array.append lower [|p|]
        let mutable upper: Point array = [||]
        let mutable i: int = (Seq.length (ps)) - 1
        while i >= 0 do
            let p: Point = _idx ps (i)
            while ((Seq.length (upper)) >= 2) && ((cross (_idx upper ((Seq.length (upper)) - 2)) (_idx upper ((Seq.length (upper)) - 1)) (p)) <= 0) do
                upper <- Array.sub upper 0 (((Seq.length (upper)) - 1) - 0)
            upper <- Array.append upper [|p|]
            i <- i - 1
        let mutable hull: Point array = Array.sub lower 0 (((Seq.length (lower)) - 1) - 0)
        let mutable j: int = 0
        while j < ((Seq.length (upper)) - 1) do
            hull <- Array.append hull [|_idx upper (j)|]
            j <- j + 1
        __ret <- hull
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
