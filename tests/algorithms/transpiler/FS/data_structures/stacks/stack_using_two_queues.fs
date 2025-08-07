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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type StackWithQueues = {
    main_queue: int array
    temp_queue: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_stack () =
    let mutable __ret : StackWithQueues = Unchecked.defaultof<StackWithQueues>
    try
        __ret <- { main_queue = [||]; temp_queue = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
let rec push (s: StackWithQueues) (item: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable item = item
    try
        s <- { s with temp_queue = Array.append (s.temp_queue) [|item|] }
        while (Seq.length (s.main_queue)) > 0 do
            s <- { s with temp_queue = Array.append (s.temp_queue) [|_idx (s.main_queue) (0)|] }
            s <- { s with main_queue = Array.sub s.main_queue 1 ((Seq.length (s.main_queue)) - 1) }
        let new_main: int array = s.temp_queue
        s <- { s with temp_queue = s.main_queue }
        s <- { s with main_queue = new_main }
        __ret
    with
        | Return -> __ret
let rec pop (s: StackWithQueues) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if (Seq.length (s.main_queue)) = 0 then
            failwith ("pop from empty stack")
        let item: int = _idx (s.main_queue) (0)
        s <- { s with main_queue = Array.sub s.main_queue 1 ((Seq.length (s.main_queue)) - 1) }
        __ret <- item
        raise Return
        __ret
    with
        | Return -> __ret
let rec peek (s: StackWithQueues) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if (Seq.length (s.main_queue)) = 0 then
            failwith ("peek from empty stack")
        __ret <- _idx (s.main_queue) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_empty (s: StackWithQueues) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- (Seq.length (s.main_queue)) = 0
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
