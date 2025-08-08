// Generated 2025-08-08 11:10 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec list_to_string (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            __ret <- ""
            raise Return
        let mutable s: string = _str (_idx xs (0))
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            s <- (s + "->") + (_str (_idx xs (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and insert_node (xs: int array) (data: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable data = data
    try
        __ret <- Array.append xs [|data|]
        raise Return
        __ret
    with
        | Return -> __ret
and rotate_to_the_right (xs: int array) (places: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable places = places
    try
        if (Seq.length (xs)) = 0 then
            failwith ("The linked list is empty.")
        let n: int = Seq.length (xs)
        let mutable k: int = ((places % n + n) % n)
        if k = 0 then
            __ret <- xs
            raise Return
        let split: int = n - k
        let mutable res: int array = [||]
        let mutable i: int = split
        while i < n do
            res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        let mutable j: int = 0
        while j < split do
            res <- Array.append res [|(_idx xs (j))|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable head: int array = [||]
        head <- insert_node (head) (5)
        head <- insert_node (head) (1)
        head <- insert_node (head) (2)
        head <- insert_node (head) (4)
        head <- insert_node (head) (3)
        printfn "%s" ("Original list: " + (list_to_string (head)))
        let places: int = 3
        let new_head: int array = rotate_to_the_right (head) (places)
        printfn "%s" ((("After " + (_str (places))) + " iterations: ") + (list_to_string (new_head)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
