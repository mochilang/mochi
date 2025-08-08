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
type List = {
    mutable _data: int array
}
let rec empty_list () =
    let mutable __ret : List = Unchecked.defaultof<List>
    try
        __ret <- { _data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and push (lst: List) (value: int) =
    let mutable __ret : List = Unchecked.defaultof<List>
    let mutable lst = lst
    let mutable value = value
    try
        let mutable res: int array = [|value|]
        let mutable i: int = 0
        while i < (Seq.length (lst._data)) do
            res <- Array.append res [|(_idx (lst._data) (i))|]
            i <- i + 1
        __ret <- { _data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and middle_element (lst: List) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    try
        let n: int = Seq.length (lst._data)
        if n = 0 then
            printfn "%s" ("No element found.")
            __ret <- 0
            raise Return
        let mutable slow: int = 0
        let mutable fast: int = 0
        while (fast + 1) < n do
            fast <- fast + 2
            slow <- slow + 1
        __ret <- _idx (lst._data) (slow)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable lst: List = empty_list()
        middle_element (lst)
        lst <- push (lst) (5)
        printfn "%d" (5)
        lst <- push (lst) (6)
        printfn "%d" (6)
        lst <- push (lst) (8)
        printfn "%d" (8)
        lst <- push (lst) (8)
        printfn "%d" (8)
        lst <- push (lst) (10)
        printfn "%d" (10)
        lst <- push (lst) (12)
        printfn "%d" (12)
        lst <- push (lst) (17)
        printfn "%d" (17)
        lst <- push (lst) (7)
        printfn "%d" (7)
        lst <- push (lst) (3)
        printfn "%d" (3)
        lst <- push (lst) (20)
        printfn "%d" (20)
        lst <- push (lst) (-20)
        printfn "%d" (-20)
        printfn "%d" (middle_element (lst))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
