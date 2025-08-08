// Generated 2025-08-08 16:34 +0700

exception Break
exception Continue

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
let rec calc_profit (profit: int array) (weight: int array) (max_weight: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable profit = profit
    let mutable weight = weight
    let mutable max_weight = max_weight
    try
        if (Seq.length (profit)) <> (Seq.length (weight)) then
            failwith ("The length of profit and weight must be same.")
        if max_weight <= 0 then
            failwith ("max_weight must greater than zero.")
        let mutable i: int = 0
        while i < (Seq.length (profit)) do
            if (_idx profit (i)) < 0 then
                failwith ("Profit can not be negative.")
            if (_idx weight (i)) < 0 then
                failwith ("Weight can not be negative.")
            i <- i + 1
        let n: int = Seq.length (profit)
        let mutable used: bool array = Array.empty<bool>
        let mutable j: int = 0
        while j < n do
            used <- Array.append used [|false|]
            j <- j + 1
        let mutable limit: int = 0
        let mutable gain: float = 0.0
        let mutable count: int = 0
        try
            while (limit < max_weight) && (count < n) do
                try
                    let mutable maxRatio: float = -1.0
                    let mutable maxIndex: int = -1
                    let mutable k: int = 0
                    while k < n do
                        if not (_idx used (k)) then
                            let ratio: float = (float (_idx profit (k))) / (float (_idx weight (k)))
                            if ratio > maxRatio then
                                maxRatio <- ratio
                                maxIndex <- k
                        k <- k + 1
                    if maxIndex < 0 then
                        raise Break
                    used.[maxIndex] <- true
                    if (max_weight - limit) >= (_idx weight (maxIndex)) then
                        limit <- limit + (_idx weight (maxIndex))
                        gain <- gain + (float (_idx profit (maxIndex)))
                    else
                        gain <- gain + (((float (max_weight - limit)) / (float (_idx weight (maxIndex)))) * (float (_idx profit (maxIndex))))
                        raise Break
                    count <- count + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- gain
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%g" (calc_profit (unbox<int array> [|1; 2; 3|]) (unbox<int array> [|3; 4; 5|]) (15))
        printfn "%g" (calc_profit (unbox<int array> [|10; 9; 8|]) (unbox<int array> [|3; 4; 5|]) (25))
        printfn "%g" (calc_profit (unbox<int array> [|10; 9; 8|]) (unbox<int array> [|3; 4; 5|]) (5))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
