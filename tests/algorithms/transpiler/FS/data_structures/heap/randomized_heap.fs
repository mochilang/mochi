// Generated 2025-08-07 14:57 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let NIL: int = 0 - 1
let mutable _seed: int = 1
let rec set_seed (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        _seed <- s
        __ret
    with
        | Return -> __ret
let rec randint (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (((_seed % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1))) + a
        raise Return
        __ret
    with
        | Return -> __ret
let rec rand_bool () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        __ret <- (randint (0) (1)) = 1
        raise Return
        __ret
    with
        | Return -> __ret
let mutable nodes: System.Collections.Generic.IDictionary<string, int> array = [||]
let mutable root: int = NIL
let rec new_heap () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        nodes <- Array.empty<System.Collections.Generic.IDictionary<string, int>>
        root <- NIL
        __ret
    with
        | Return -> __ret
let rec merge (r1: int) (r2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable r1 = r1
    let mutable r2 = r2
    try
        if r1 = NIL then
            __ret <- r2
            raise Return
        if r2 = NIL then
            __ret <- r1
            raise Return
        if (_idx nodes (r1).[(string ("value"))]) > (_idx nodes (r2).[(string ("value"))]) then
            let tmp: int = r1
            r1 <- r2
            r2 <- tmp
        if rand_bool() then
            let tmp: int = _idx nodes (r1).[(string ("left"))]
            nodes.[r1].["left"] <- _idx nodes (r1).[(string ("right"))]
            nodes.[r1].["right"] <- tmp
        nodes.[r1].["left"] <- merge (_idx nodes (r1).[(string ("left"))]) (r2)
        __ret <- r1
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable value = value
    try
        let node: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("value", value); ("left", NIL); ("right", NIL)]
        nodes <- Array.append nodes [|node|]
        let idx: int = (Seq.length (nodes)) - 1
        root <- merge (root) (idx)
        __ret
    with
        | Return -> __ret
let rec top () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- if root = NIL then 0 else (_idx nodes (root).[(string ("value"))])
        raise Return
        __ret
    with
        | Return -> __ret
let rec pop () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let result: int = top()
        let l: int = _idx nodes (root).[(string ("left"))]
        let r: int = _idx nodes (root).[(string ("right"))]
        root <- merge (l) (r)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_empty () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        __ret <- root = NIL
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_sorted_list () =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    try
        let mutable res: int array = [||]
        while not (is_empty()) do
            res <- Array.append res [|pop()|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
set_seed (1)
new_heap()
insert (2)
insert (3)
insert (1)
insert (5)
insert (1)
insert (7)
printfn "%s" (_repr (to_sorted_list()))
new_heap()
insert (1)
insert (-1)
insert (0)
printfn "%s" (_repr (to_sorted_list()))
new_heap()
insert (3)
insert (1)
insert (3)
insert (7)
printfn "%d" (pop())
printfn "%d" (pop())
printfn "%d" (pop())
printfn "%d" (pop())
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
