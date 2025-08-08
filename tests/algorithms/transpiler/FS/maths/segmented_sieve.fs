// Generated 2025-08-08 18:09 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec min_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        while ((int64 (r + 1)) * (int64 (r + 1))) <= (int64 n) do
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and sieve (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n <= 0 then
            failwith ("Number must instead be a positive integer")
        let mutable in_prime: int array = Array.empty<int>
        let mutable start: int = 2
        let ``end``: int = int_sqrt (n)
        let mutable temp: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (``end`` + 1) do
            temp <- Array.append temp [|1|]
            i <- i + 1
        let mutable prime: int array = Array.empty<int>
        while start <= ``end`` do
            if (_idx temp (start)) = 1 then
                in_prime <- Array.append in_prime [|start|]
                let mutable j: int64 = (int64 start) * (int64 start)
                while j <= (int64 ``end``) do
                    temp.[int j] <- 0
                    j <- j + (int64 start)
            start <- start + 1
        i <- 0
        while i < (Seq.length (in_prime)) do
            prime <- Array.append prime [|(_idx in_prime (i))|]
            i <- i + 1
        let mutable low: int = ``end`` + 1
        let mutable high: int = min_int (int ((int64 2) * (int64 ``end``))) (n)
        while low <= n do
            let mutable tempSeg: int array = Array.empty<int>
            let mutable size: int = (high - low) + 1
            let mutable k: int = 0
            while k < size do
                tempSeg <- Array.append tempSeg [|1|]
                k <- k + 1
            let mutable idx: int = 0
            while idx < (Seq.length (in_prime)) do
                let each: int = _idx in_prime (idx)
                let mutable t: int64 = (int64 (_floordiv low each)) * (int64 each)
                if t < (int64 low) then
                    t <- t + (int64 each)
                let mutable j2: int64 = t
                while j2 <= (int64 high) do
                    tempSeg.[int (j2 - (int64 low))] <- 0
                    j2 <- j2 + (int64 each)
                idx <- idx + 1
            let mutable j3: int = 0
            while j3 < (Seq.length (tempSeg)) do
                if (_idx tempSeg (j3)) = 1 then
                    prime <- Array.append prime [|(j3 + low)|]
                j3 <- j3 + 1
            low <- high + 1
            high <- min_int (high + ``end``) (n)
        __ret <- prime
        raise Return
        __ret
    with
        | Return -> __ret
and lists_equal (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable m: int = 0
        while m < (Seq.length (a)) do
            if (_idx a (m)) <> (_idx b (m)) then
                __ret <- false
                raise Return
            m <- m + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_sieve () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let e1: int array = sieve (8)
        if not (lists_equal (e1) (unbox<int array> [|2; 3; 5; 7|])) then
            failwith ("sieve(8) failed")
        let e2: int array = sieve (27)
        if not (lists_equal (e2) (unbox<int array> [|2; 3; 5; 7; 11; 13; 17; 19; 23|])) then
            failwith ("sieve(27) failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_sieve()
        printfn "%s" (_str (sieve (30)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
