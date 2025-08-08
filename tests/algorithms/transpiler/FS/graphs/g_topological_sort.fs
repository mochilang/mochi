// Generated 2025-08-08 16:03 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let rec depth_first_search (u: int) (visited: bool array) (graph: int array array) (stack: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable u = u
    let mutable visited = visited
    let mutable graph = graph
    let mutable stack = stack
    try
        visited.[u] <- true
        let mutable i: int = 0
        while i < (Seq.length (_idx graph (u))) do
            let v: int = _idx (_idx graph (u)) (i)
            if not (_idx visited (v)) then
                stack <- depth_first_search (v) (visited) (graph) (stack)
            i <- i + 1
        stack <- Array.append stack [|u|]
        __ret <- stack
        raise Return
        __ret
    with
        | Return -> __ret
and topological_sort (graph: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    try
        let mutable visited: bool array = [||]
        let mutable i: int = 0
        while i < (Seq.length (graph)) do
            visited <- Array.append visited [|false|]
            i <- i + 1
        let mutable stack: int array = [||]
        i <- 0
        while i < (Seq.length (graph)) do
            if not (_idx visited (i)) then
                stack <- depth_first_search (i) (visited) (graph) (stack)
            i <- i + 1
        __ret <- stack
        raise Return
        __ret
    with
        | Return -> __ret
and print_stack (stack: int array) (clothes: System.Collections.Generic.IDictionary<int, string>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable stack = stack
    let mutable clothes = clothes
    try
        let mutable order: int = 1
        let mutable s: int array = stack
        while (Seq.length (s)) > 0 do
            let idx: int = _idx s ((Seq.length (s)) - 1)
            s <- Array.sub s 0 (((Seq.length (s)) - 1) - 0)
            printfn "%s" (((_str (order)) + " ") + (_dictGet clothes (idx)))
            order <- order + 1
        __ret
    with
        | Return -> __ret
and format_list (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable res: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            res <- res + (_str (_idx xs (i)))
            if i < ((Seq.length (xs)) - 1) then
                res <- res + ", "
            i <- i + 1
        res <- res + "]"
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
        let clothes: System.Collections.Generic.IDictionary<int, string> = _dictCreate [(0, "underwear"); (1, "pants"); (2, "belt"); (3, "suit"); (4, "shoe"); (5, "socks"); (6, "shirt"); (7, "tie"); (8, "watch")]
        let graph: int array array = [|[|1; 4|]; [|2; 4|]; [|3|]; [||]; [||]; [|4|]; [|2; 7|]; [|3|]; [||]|]
        let mutable stack: int array = topological_sort (graph)
        printfn "%s" (format_list (stack))
        print_stack (stack) (clothes)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
