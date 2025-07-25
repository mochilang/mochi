// Generated 2025-07-26 05:05 +0700

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
let rec poolPut (p: int array) (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p = p
    let mutable x = x
    try
        __ret <- Array.append p [|x|]
        raise Return
        __ret
    with
        | Return -> __ret
and poolGet (p: int array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable p = p
    try
        if (unbox<int> (Array.length p)) = 0 then
            printfn "%s" "pool empty"
            __ret <- Map.ofList [("pool", box p); ("val", box 0)]
            raise Return
        let idx: int = (unbox<int> (Array.length p)) - 1
        let v: int = p.[idx]
        p <- Array.sub p 0 (idx - 0)
        __ret <- Map.ofList [("pool", box p); ("val", box v)]
        raise Return
        __ret
    with
        | Return -> __ret
and clearPool (p: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p = p
    try
        __ret <- [||]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable pool: int array = [||]
        let mutable i: int = 1
        let mutable j: int = 2
        printfn "%s" (string (i + j))
        pool <- poolPut pool i
        pool <- poolPut pool j
        i <- 0
        j <- 0
        let res1: Map<string, obj> = poolGet pool
        pool <- unbox<int array> (res1.["pool"])
        i <- unbox<int> (res1.["val"])
        let res2: Map<string, obj> = poolGet pool
        pool <- unbox<int array> (res2.["pool"])
        j <- unbox<int> (res2.["val"])
        i <- 4
        j <- 5
        printfn "%s" (string (i + j))
        pool <- poolPut pool i
        pool <- poolPut pool j
        i <- 0
        j <- 0
        pool <- clearPool pool
        let res3: Map<string, obj> = poolGet pool
        pool <- unbox<int array> (res3.["pool"])
        i <- unbox<int> (res3.["val"])
        let res4: Map<string, obj> = poolGet pool
        pool <- unbox<int array> (res4.["pool"])
        j <- unbox<int> (res4.["val"])
        i <- 7
        j <- 8
        printfn "%s" (string (i + j))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
