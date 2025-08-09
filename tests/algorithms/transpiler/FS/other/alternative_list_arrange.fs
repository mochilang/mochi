// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Item =
    | Int of int
    | Str of string
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec from_int (x: int) =
    let mutable __ret : Item = Unchecked.defaultof<Item>
    let mutable x = x
    try
        __ret <- Int(x)
        raise Return
        __ret
    with
        | Return -> __ret
let rec from_string (s: string) =
    let mutable __ret : Item = Unchecked.defaultof<Item>
    let mutable s = s
    try
        __ret <- Str(s)
        raise Return
        __ret
    with
        | Return -> __ret
let rec item_to_string (it: Item) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable it = it
    try
        __ret <- unbox<string> ((match it with
            | Int(v) -> _str (v)
            | Str(s) -> s))
        raise Return
        __ret
    with
        | Return -> __ret
let rec alternative_list_arrange (first: Item array) (second: Item array) =
    let mutable __ret : Item array = Unchecked.defaultof<Item array>
    let mutable first = first
    let mutable second = second
    try
        let len1: int = Seq.length (first)
        let len2: int = Seq.length (second)
        let abs_len: int = if len1 > len2 then len1 else len2
        let mutable result: Item array = Array.empty<Item>
        let mutable i: int = 0
        while i < abs_len do
            if i < len1 then
                result <- Array.append result [|(_idx first (int i))|]
            if i < len2 then
                result <- Array.append result [|(_idx second (int i))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (xs: Item array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (item_to_string (_idx xs (int i)))
            if i < ((Seq.length (xs)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let example1: Item array = alternative_list_arrange (unbox<Item array> [|from_int (1); from_int (2); from_int (3); from_int (4); from_int (5)|]) (unbox<Item array> [|from_string ("A"); from_string ("B"); from_string ("C")|])
printfn "%s" (list_to_string (example1))
let example2: Item array = alternative_list_arrange (unbox<Item array> [|from_string ("A"); from_string ("B"); from_string ("C")|]) (unbox<Item array> [|from_int (1); from_int (2); from_int (3); from_int (4); from_int (5)|])
printfn "%s" (list_to_string (example2))
let example3: Item array = alternative_list_arrange (unbox<Item array> [|from_string ("X"); from_string ("Y"); from_string ("Z")|]) (unbox<Item array> [|from_int (9); from_int (8); from_int (7); from_int (6)|])
printfn "%s" (list_to_string (example3))
let example4: Item array = alternative_list_arrange (unbox<Item array> [|from_int (1); from_int (2); from_int (3); from_int (4); from_int (5)|]) (Array.empty<Item>)
printfn "%s" (list_to_string (example4))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
