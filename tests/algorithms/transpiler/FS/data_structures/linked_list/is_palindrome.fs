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
let rec is_palindrome (values: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable values = values
    try
        let mutable stack: int array = [||]
        let mutable fast: int = 0
        let mutable slow: int = 0
        let n: int = Seq.length (values)
        while (fast < n) && ((fast + 1) < n) do
            stack <- Array.append stack [|(_idx values (slow))|]
            slow <- slow + 1
            fast <- fast + 2
        if fast = (n - 1) then
            slow <- slow + 1
        let mutable i: int = (Seq.length (stack)) - 1
        while slow < n do
            if (_idx stack (i)) <> (_idx values (slow)) then
                __ret <- false
                raise Return
            i <- i - 1
            slow <- slow + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%b" (is_palindrome (Array.empty<int>))
        printfn "%b" (is_palindrome (unbox<int array> [|1|]))
        printfn "%b" (is_palindrome (unbox<int array> [|1; 2|]))
        printfn "%b" (is_palindrome (unbox<int array> [|1; 2; 1|]))
        printfn "%b" (is_palindrome (unbox<int array> [|1; 2; 2; 1|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
