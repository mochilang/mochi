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
type Deque = {
    data: int array
}
type PopResult = {
    deque: Deque
    value: int
}
let rec empty_deque () =
    let mutable __ret : Deque = Unchecked.defaultof<Deque>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and push_back (dq: Deque) (value: int) =
    let mutable __ret : Deque = Unchecked.defaultof<Deque>
    let mutable dq = dq
    let mutable value = value
    try
        __ret <- { data = Array.append (dq.data) [|value|] }
        raise Return
        __ret
    with
        | Return -> __ret
and push_front (dq: Deque) (value: int) =
    let mutable __ret : Deque = Unchecked.defaultof<Deque>
    let mutable dq = dq
    let mutable value = value
    try
        let mutable res: int array = [|value|]
        let mutable i: int = 0
        while i < (Seq.length (dq.data)) do
            res <- Array.append res [|_idx (dq.data) (i)|]
            i <- i + 1
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and extend_back (dq: Deque) (values: int array) =
    let mutable __ret : Deque = Unchecked.defaultof<Deque>
    let mutable dq = dq
    let mutable values = values
    try
        let mutable res: int array = dq.data
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            res <- Array.append res [|_idx values (i)|]
            i <- i + 1
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and extend_front (dq: Deque) (values: int array) =
    let mutable __ret : Deque = Unchecked.defaultof<Deque>
    let mutable dq = dq
    let mutable values = values
    try
        let mutable res: int array = [||]
        let mutable i: int = (Seq.length (values)) - 1
        while i >= 0 do
            res <- Array.append res [|_idx values (i)|]
            i <- i - 1
        let mutable j: int = 0
        while j < (Seq.length (dq.data)) do
            res <- Array.append res [|_idx (dq.data) (j)|]
            j <- j + 1
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and pop_back (dq: Deque) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable dq = dq
    try
        if (Seq.length (dq.data)) = 0 then
            failwith ("pop from empty deque")
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (dq.data)) - 1) do
            res <- Array.append res [|_idx (dq.data) (i)|]
            i <- i + 1
        __ret <- { deque = { data = res }; value = _idx (dq.data) ((Seq.length (dq.data)) - 1) }
        raise Return
        __ret
    with
        | Return -> __ret
and pop_front (dq: Deque) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable dq = dq
    try
        if (Seq.length (dq.data)) = 0 then
            failwith ("popleft from empty deque")
        let mutable res: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (dq.data)) do
            res <- Array.append res [|_idx (dq.data) (i)|]
            i <- i + 1
        __ret <- { deque = { data = res }; value = _idx (dq.data) (0) }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (dq: Deque) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable dq = dq
    try
        __ret <- (Seq.length (dq.data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and length (dq: Deque) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable dq = dq
    try
        __ret <- Seq.length (dq.data)
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (dq: Deque) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable dq = dq
    try
        if (Seq.length (dq.data)) = 0 then
            __ret <- "[]"
            raise Return
        let mutable s: string = "[" + (_str (_idx (dq.data) (0)))
        let mutable i: int = 1
        while i < (Seq.length (dq.data)) do
            s <- (s + ", ") + (_str (_idx (dq.data) (i)))
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable dq: Deque = empty_deque()
        dq <- push_back (dq) (2)
        dq <- push_front (dq) (1)
        dq <- extend_back (dq) (unbox<int array> [|3; 4|])
        dq <- extend_front (dq) (unbox<int array> [|0|])
        printfn "%s" (to_string (dq))
        let mutable r: PopResult = pop_back (dq)
        dq <- r.deque
        printfn "%d" (r.value)
        r <- pop_front (dq)
        dq <- r.deque
        printfn "%d" (r.value)
        printfn "%s" (to_string (dq))
        printfn "%b" (is_empty (empty_deque()))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
