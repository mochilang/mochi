// Generated 2025-08-08 17:35 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec floor_div (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable q: int = _floordiv a b
        let r: int = ((a % b + b) % b)
        if (r <> 0) && (((a < 0) && (b > 0)) || ((a > 0) && (b < 0))) then
            q <- q - 1
        __ret <- q
        raise Return
        __ret
    with
        | Return -> __ret
let rec continued_fraction (numerator: int) (denominator: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable numerator = numerator
    let mutable denominator = denominator
    try
        let mutable num: int = numerator
        let mutable den: int = denominator
        let mutable result: int array = Array.empty<int>
        try
            while true do
                try
                    let integer_part: int = floor_div (num) (den)
                    result <- Array.append result [|integer_part|]
                    num <- num - (integer_part * den)
                    if num = 0 then
                        raise Break
                    let tmp: int = num
                    num <- den
                    den <- tmp
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (lst: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            s <- s + (_str (_idx lst (i)))
            if i < ((Seq.length (lst)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("Continued Fraction of 0.84375 is: " + (list_to_string (continued_fraction (27) (32))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
