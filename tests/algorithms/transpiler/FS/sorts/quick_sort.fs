// Generated 2025-08-11 17:23 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec quick_sort (items: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable items = items
    try
        if (Seq.length (items)) < 2 then
            __ret <- items
            raise Return
        let pivot: int = _idx items (int 0)
        let mutable lesser: int array = Array.empty<int>
        let mutable greater: int array = Array.empty<int>
        let mutable i: int = 1
        while i < (Seq.length (items)) do
            let item: int = _idx items (int i)
            if item <= pivot then
                lesser <- Array.append lesser [|item|]
            else
                greater <- Array.append greater [|item|]
            i <- i + 1
        __ret <- Array.append (Array.append (quick_sort (lesser)) ([|pivot|])) (quick_sort (greater))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (String.concat " " ([|sprintf "%s" ("sorted1:"); sprintf "%A" (quick_sort (unbox<int array> [|0; 5; 3; 2; 2|]))|]))
printfn "%s" (String.concat " " ([|sprintf "%s" ("sorted2:"); sprintf "%A" (quick_sort (Array.empty<int>))|]))
printfn "%s" (String.concat " " ([|sprintf "%s" ("sorted3:"); sprintf "%A" (quick_sort (unbox<int array> [|-2; 5; 0; -45|]))|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
