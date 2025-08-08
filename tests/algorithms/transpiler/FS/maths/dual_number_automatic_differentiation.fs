// Generated 2025-08-08 17:35 +0700

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
type Dual = {
    mutable _real: float
    mutable _duals: float array
}
let rec make_dual (_real: float) (rank: int) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable _real = _real
    let mutable rank = rank
    try
        let mutable ds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < rank do
            ds <- Array.append ds [|1.0|]
            i <- i + 1
        __ret <- { _real = _real; _duals = ds }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_from_list (_real: float) (ds: float array) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable _real = _real
    let mutable ds = ds
    try
        __ret <- { _real = _real; _duals = ds }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_add (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        let mutable s_dual: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a._duals)) do
            s_dual <- Array.append s_dual [|(_idx (a._duals) (i))|]
            i <- i + 1
        let mutable o_dual: float array = Array.empty<float>
        let mutable j: int = 0
        while j < (Seq.length (b._duals)) do
            o_dual <- Array.append o_dual [|(_idx (b._duals) (j))|]
            j <- j + 1
        if (Seq.length (s_dual)) > (Seq.length (o_dual)) then
            let mutable diff: int = (Seq.length (s_dual)) - (Seq.length (o_dual))
            let mutable k: int = 0
            while k < diff do
                o_dual <- Array.append o_dual [|1.0|]
                k <- k + 1
        else
            if (Seq.length (s_dual)) < (Seq.length (o_dual)) then
                let mutable diff2: int = (Seq.length (o_dual)) - (Seq.length (s_dual))
                let mutable k2: int = 0
                while k2 < diff2 do
                    s_dual <- Array.append s_dual [|1.0|]
                    k2 <- k2 + 1
        let mutable new_duals: float array = Array.empty<float>
        let mutable idx: int = 0
        while idx < (Seq.length (s_dual)) do
            new_duals <- Array.append new_duals [|((_idx s_dual (idx)) + (_idx o_dual (idx)))|]
            idx <- idx + 1
        __ret <- { _real = (a._real) + (b._real); _duals = new_duals }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_add_real (a: Dual) (b: float) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        let mutable ds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a._duals)) do
            ds <- Array.append ds [|(_idx (a._duals) (i))|]
            i <- i + 1
        __ret <- { _real = (a._real) + b; _duals = ds }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_mul (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        let new_len: int = ((Seq.length (a._duals)) + (Seq.length (b._duals))) + 1
        let mutable new_duals: float array = Array.empty<float>
        let mutable idx: int = 0
        while idx < new_len do
            new_duals <- Array.append new_duals [|0.0|]
            idx <- idx + 1
        let mutable i: int = 0
        while i < (Seq.length (a._duals)) do
            let mutable j: int = 0
            while j < (Seq.length (b._duals)) do
                let pos: int = (i + j) + 1
                let ``val``: float = (_idx new_duals (pos)) + ((_idx (a._duals) (i)) * (_idx (b._duals) (j)))
                new_duals.[pos] <- ``val``
                j <- j + 1
            i <- i + 1
        let mutable k: int = 0
        while k < (Seq.length (a._duals)) do
            let ``val``: float = (_idx new_duals (k)) + ((_idx (a._duals) (k)) * (b._real))
            new_duals.[k] <- ``val``
            k <- k + 1
        let mutable l: int = 0
        while l < (Seq.length (b._duals)) do
            let ``val``: float = (_idx new_duals (l)) + ((_idx (b._duals) (l)) * (a._real))
            new_duals.[l] <- ``val``
            l <- l + 1
        __ret <- { _real = (a._real) * (b._real); _duals = new_duals }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_mul_real (a: Dual) (b: float) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        let mutable ds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a._duals)) do
            ds <- Array.append ds [|((_idx (a._duals) (i)) * b)|]
            i <- i + 1
        __ret <- { _real = (a._real) * b; _duals = ds }
        raise Return
        __ret
    with
        | Return -> __ret
and dual_pow (x: Dual) (n: int) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable x = x
    let mutable n = n
    try
        if n < 0 then
            failwith ("power must be a positive integer")
        if n = 0 then
            __ret <- { _real = 1.0; _duals = Array.empty<float> }
            raise Return
        let mutable res: Dual = x
        let mutable i: int = 1
        while i < n do
            res <- dual_mul (res) (x)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and factorial (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable res: float = 1.0
        let mutable i: int = 2
        while i <= n do
            res <- res * (float i)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and differentiate (func: Dual -> Dual) (position: float) (order: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable func = func
    let mutable position = position
    let mutable order = order
    try
        let d: Dual = make_dual (position) (1)
        let result: Dual = func (d)
        if order = 0 then
            __ret <- result._real
            raise Return
        __ret <- (_idx (result._duals) (order - 1)) * (factorial (order))
        raise Return
        __ret
    with
        | Return -> __ret
and test_differentiate () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let rec f1 (x: Dual) =
            let mutable __ret : Dual = Unchecked.defaultof<Dual>
            let mutable x = x
            try
                __ret <- dual_pow (x) (2)
                raise Return
                __ret
            with
                | Return -> __ret
        if (differentiate (unbox<Dual -> Dual> f1) (2.0) (2)) <> 2.0 then
            failwith ("f1 failed")
        let rec f2 (x: Dual) =
            let mutable __ret : Dual = Unchecked.defaultof<Dual>
            let mutable x = x
            try
                __ret <- dual_mul (dual_pow (x) (2)) (dual_pow (x) (4))
                raise Return
                __ret
            with
                | Return -> __ret
        if (differentiate (unbox<Dual -> Dual> f2) (9.0) (2)) <> 196830.0 then
            failwith ("f2 failed")
        let rec f3 (y: Dual) =
            let mutable __ret : Dual = Unchecked.defaultof<Dual>
            let mutable y = y
            try
                __ret <- dual_mul_real (dual_pow (dual_add_real (y) (3.0)) (6)) (0.5)
                raise Return
                __ret
            with
                | Return -> __ret
        if (differentiate (unbox<Dual -> Dual> f3) (3.5) (4)) <> 7605.0 then
            failwith ("f3 failed")
        let rec f4 (y: Dual) =
            let mutable __ret : Dual = Unchecked.defaultof<Dual>
            let mutable y = y
            try
                __ret <- dual_pow (y) (2)
                raise Return
                __ret
            with
                | Return -> __ret
        if (differentiate (unbox<Dual -> Dual> f4) (4.0) (3)) <> 0.0 then
            failwith ("f4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_differentiate()
        let rec f (y: Dual) =
            let mutable __ret : Dual = Unchecked.defaultof<Dual>
            let mutable y = y
            try
                __ret <- dual_mul (dual_pow (y) (2)) (dual_pow (y) (4))
                raise Return
                __ret
            with
                | Return -> __ret
        let mutable res: float = differentiate (unbox<Dual -> Dual> f) (9.0) (2)
        printfn "%g" (res)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
