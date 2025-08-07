// Generated 2025-08-07 17:32 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type FuzzySet = {
    name: string
    left_boundary: float
    peak: float
    right_boundary: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec stringify (fs: FuzzySet) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable fs = fs
    try
        __ret <- (((((((fs.name) + ": [") + (_str (fs.left_boundary))) + ", ") + (_str (fs.peak))) + ", ") + (_str (fs.right_boundary))) + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec max2 (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec min2 (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec complement (fs: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable fs = fs
    try
        __ret <- { name = "¬" + (fs.name); left_boundary = 1.0 - (fs.right_boundary); peak = 1.0 - (fs.left_boundary); right_boundary = 1.0 - (fs.peak) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec intersection (a: FuzzySet) (b: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { name = ((a.name) + " ∩ ") + (b.name); left_boundary = max2 (a.left_boundary) (b.left_boundary); peak = min2 (a.right_boundary) (b.right_boundary); right_boundary = ((a.peak) + (b.peak)) / 2.0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec union (a: FuzzySet) (b: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { name = ((a.name) + " U ") + (b.name); left_boundary = min2 (a.left_boundary) (b.left_boundary); peak = max2 (a.right_boundary) (b.right_boundary); right_boundary = ((a.peak) + (b.peak)) / 2.0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec membership (fs: FuzzySet) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fs = fs
    let mutable x = x
    try
        if (x <= (fs.left_boundary)) || (x >= (fs.right_boundary)) then
            __ret <- 0.0
            raise Return
        if ((fs.left_boundary) < x) && (x <= (fs.peak)) then
            __ret <- (x - (fs.left_boundary)) / ((fs.peak) - (fs.left_boundary))
            raise Return
        if ((fs.peak) < x) && (x < (fs.right_boundary)) then
            __ret <- ((fs.right_boundary) - x) / ((fs.right_boundary) - (fs.peak))
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let sheru: FuzzySet = { name = "Sheru"; left_boundary = 0.4; peak = 1.0; right_boundary = 0.6 }
let siya: FuzzySet = { name = "Siya"; left_boundary = 0.5; peak = 1.0; right_boundary = 0.7 }
printfn "%s" (stringify (sheru))
printfn "%s" (stringify (siya))
let sheru_comp: FuzzySet = complement (sheru)
printfn "%s" (stringify (sheru_comp))
let inter: FuzzySet = intersection (siya) (sheru)
printfn "%s" (stringify (inter))
printfn "%s" ("Sheru membership 0.5: " + (_str (membership (sheru) (0.5))))
printfn "%s" ("Sheru membership 0.6: " + (_str (membership (sheru) (0.6))))
let uni: FuzzySet = union (siya) (sheru)
printfn "%s" (stringify (uni))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
