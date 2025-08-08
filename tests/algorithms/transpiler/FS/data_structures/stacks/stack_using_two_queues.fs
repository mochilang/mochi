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
type StackWithQueues = {
    mutable _main_queue: int array
    mutable _temp_queue: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_stack () =
    let mutable __ret : StackWithQueues = Unchecked.defaultof<StackWithQueues>
    try
        __ret <- { _main_queue = [||]; _temp_queue = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
let rec push (s: StackWithQueues) (item: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable item = item
    try
        s._temp_queue <- Array.append (s._temp_queue) [|item|]
        while (Seq.length (s._main_queue)) > 0 do
            s._temp_queue <- Array.append (s._temp_queue) [|(_idx (s._main_queue) (0))|]
            s._main_queue <- Array.sub s._main_queue 1 ((Seq.length (s._main_queue)) - 1)
        let new_main: int array = s._temp_queue
        s._temp_queue <- s._main_queue
        s._main_queue <- new_main
        __ret
    with
        | Return -> __ret
let rec pop (s: StackWithQueues) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if (Seq.length (s._main_queue)) = 0 then
            failwith ("pop from empty stack")
        let item: int = _idx (s._main_queue) (0)
        s._main_queue <- Array.sub s._main_queue 1 ((Seq.length (s._main_queue)) - 1)
        __ret <- item
        raise Return
        __ret
    with
        | Return -> __ret
let rec peek (s: StackWithQueues) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if (Seq.length (s._main_queue)) = 0 then
            failwith ("peek from empty stack")
        __ret <- _idx (s._main_queue) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_empty (s: StackWithQueues) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- (Seq.length (s._main_queue)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let mutable stack: StackWithQueues = make_stack()
push (stack) (1)
push (stack) (2)
push (stack) (3)
printfn "%s" (_str (peek (stack)))
printfn "%s" (_str (pop (stack)))
printfn "%s" (_str (peek (stack)))
printfn "%s" (_str (pop (stack)))
printfn "%s" (_str (pop (stack)))
printfn "%s" (_str (is_empty (stack)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
