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
type CalcResult = {
    mutable _ok: bool
    mutable _value: float
    mutable _error: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec calc_profit (profit: int array) (weight: int array) (max_weight: int) =
    let mutable __ret : CalcResult = Unchecked.defaultof<CalcResult>
    let mutable profit = profit
    let mutable weight = weight
    let mutable max_weight = max_weight
    try
        if (Seq.length (profit)) <> (Seq.length (weight)) then
            __ret <- { _ok = false; _value = 0.0; _error = "The length of profit and weight must be same." }
            raise Return
        if max_weight <= 0 then
            __ret <- { _ok = false; _value = 0.0; _error = "max_weight must greater than zero." }
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (profit)) do
            if (_idx profit (i)) < 0 then
                __ret <- { _ok = false; _value = 0.0; _error = "Profit can not be negative." }
                raise Return
            if (_idx weight (i)) < 0 then
                __ret <- { _ok = false; _value = 0.0; _error = "Weight can not be negative." }
                raise Return
            i <- i + 1
        let mutable used: bool array = Array.empty<bool>
        let mutable j: int = 0
        while j < (Seq.length (profit)) do
            used <- Array.append used [|false|]
            j <- j + 1
        let mutable limit: int = 0
        let mutable gain: float = 0.0
        try
            while limit < max_weight do
                try
                    let mutable max_ratio: float = -1.0
                    let mutable idx: int = 0 - 1
                    let mutable k: int = 0
                    while k < (Seq.length (profit)) do
                        if not (_idx used (k)) then
                            let ratio: float = (float (_idx profit (k))) / (float (_idx weight (k)))
                            if ratio > max_ratio then
                                max_ratio <- ratio
                                idx <- k
                        k <- k + 1
                    if idx = (0 - 1) then
                        raise Break
                    used.[idx] <- true
                    if (max_weight - limit) >= (_idx weight (idx)) then
                        limit <- limit + (_idx weight (idx))
                        gain <- gain + (float (_idx profit (idx)))
                    else
                        gain <- gain + (((float (max_weight - limit)) / (float (_idx weight (idx)))) * (float (_idx profit (idx))))
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _ok = true; _value = gain; _error = "" }
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_sorted () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; 20; 30; 40; 50; 60|]
        let weight: int array = unbox<int array> [|2; 4; 6; 8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (100)
        __ret <- (res._ok) && ((res._value) = 210.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_negative_max_weight () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; 20; 30; 40; 50; 60|]
        let weight: int array = unbox<int array> [|2; 4; 6; 8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (-15)
        __ret <- (not (res._ok)) && ((res._error) = "max_weight must greater than zero.")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_negative_profit_value () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; -20; 30; 40; 50; 60|]
        let weight: int array = unbox<int array> [|2; 4; 6; 8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (15)
        __ret <- (not (res._ok)) && ((res._error) = "Profit can not be negative.")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_negative_weight_value () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; 20; 30; 40; 50; 60|]
        let weight: int array = unbox<int array> [|2; -4; 6; -8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (15)
        __ret <- (not (res._ok)) && ((res._error) = "Weight can not be negative.")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_null_max_weight () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; 20; 30; 40; 50; 60|]
        let weight: int array = unbox<int array> [|2; 4; 6; 8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (0)
        __ret <- (not (res._ok)) && ((res._error) = "max_weight must greater than zero.")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_unequal_list_length () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let profit: int array = unbox<int array> [|10; 20; 30; 40; 50|]
        let weight: int array = unbox<int array> [|2; 4; 6; 8; 10; 12|]
        let res: CalcResult = calc_profit (profit) (weight) (100)
        __ret <- (not (res._ok)) && ((res._error) = "The length of profit and weight must be same.")
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (test_sorted())
printfn "%b" (test_negative_max_weight())
printfn "%b" (test_negative_profit_value())
printfn "%b" (test_negative_weight_value())
printfn "%b" (test_null_max_weight())
printfn "%b" (test_unequal_list_length())
printfn "%b" (true)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
