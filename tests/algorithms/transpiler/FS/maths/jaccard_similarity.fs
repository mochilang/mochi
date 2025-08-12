// Generated 2025-08-12 07:47 +0700

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
let rec contains (xs: string array) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and jaccard_similarity (set_a: string array) (set_b: string array) (alternative_union: bool) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable set_a = set_a
    let mutable set_b = set_b
    let mutable alternative_union = alternative_union
    try
        let mutable intersection_len: int = 0
        let mutable i: int = 0
        while i < (Seq.length (set_a)) do
            if contains (set_b) (_idx set_a (int i)) then
                intersection_len <- intersection_len + 1
            i <- i + 1
        let mutable union_len: int = 0
        if alternative_union then
            union_len <- (Seq.length (set_a)) + (Seq.length (set_b))
        else
            let mutable union_list: string array = Array.empty<string>
            i <- 0
            while i < (Seq.length (set_a)) do
                let val_a: string = _idx set_a (int i)
                if not (contains (union_list) (val_a)) then
                    union_list <- Array.append union_list [|val_a|]
                i <- i + 1
            i <- 0
            while i < (Seq.length (set_b)) do
                let val_b: string = _idx set_b (int i)
                if not (contains (union_list) (val_b)) then
                    union_list <- Array.append union_list [|val_b|]
                i <- i + 1
            union_len <- Seq.length (union_list)
        __ret <- (1.0 * (float intersection_len)) / (float union_len)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let set_a: string array = unbox<string array> [|"a"; "b"; "c"; "d"; "e"|]
        let set_b: string array = unbox<string array> [|"c"; "d"; "e"; "f"; "h"; "i"|]
        printfn "%g" (jaccard_similarity (set_a) (set_b) (false))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
