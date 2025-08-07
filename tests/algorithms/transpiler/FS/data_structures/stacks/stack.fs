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
type Stack = {
    items: int array
    limit: int
}
let rec make_stack (limit: int) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable limit = limit
    try
        __ret <- { items = [||]; limit = limit }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (s: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- (Seq.length (s.items)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and size (s: Stack) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        __ret <- Seq.length (s.items)
        raise Return
        __ret
    with
        | Return -> __ret
and is_full (s: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- (Seq.length (s.items)) >= (s.limit)
        raise Return
        __ret
    with
        | Return -> __ret
and push (s: Stack) (item: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable item = item
    try
        if is_full (s) then
            failwith ("stack overflow")
        s <- { s with items = Array.append (s.items) [|item|] }
        __ret
    with
        | Return -> __ret
and pop (s: Stack) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if is_empty (s) then
            failwith ("stack underflow")
        let n: int = Seq.length (s.items)
        let ``val``: int = _idx (s.items) (n - 1)
        s <- { s with items = Array.sub s.items 0 ((n - 1) - 0) }
        __ret <- ``val``
        raise Return
        __ret
    with
        | Return -> __ret
and peek (s: Stack) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if is_empty (s) then
            failwith ("peek from empty stack")
        __ret <- _idx (s.items) ((Seq.length (s.items)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and contains (s: Stack) (item: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable item = item
    try
        let mutable i: int = 0
        while i < (Seq.length (s.items)) do
            if (_idx (s.items) (i)) = item then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and stack_repr (s: Stack) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- _str (s.items)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable s: Stack = make_stack (5)
        printfn "%s" (_str (is_empty (s)))
        push (s) (0)
        push (s) (1)
        push (s) (2)
        printfn "%s" (_str (peek (s)))
        printfn "%s" (_str (size (s)))
        printfn "%s" (_str (is_full (s)))
        push (s) (3)
        push (s) (4)
        printfn "%s" (_str (is_full (s)))
        printfn "%s" (stack_repr (s))
        printfn "%s" (_str (pop (s)))
        printfn "%s" (_str (peek (s)))
        printfn "%s" (_str (contains (s) (1)))
        printfn "%s" (_str (contains (s) (9)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
