// Generated 2025-08-13 16:13 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type FuzzySet = {
    mutable _name: string
    mutable _left_boundary: float
    mutable _peak: float
    mutable _right_boundary: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec stringify (fs: FuzzySet) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable fs = fs
    try
        __ret <- (((((((fs._name) + ": [") + (_str (fs._left_boundary))) + ", ") + (_str (fs._peak))) + ", ") + (_str (fs._right_boundary))) + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and max2 (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and min2 (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and complement (fs: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable fs = fs
    try
        __ret <- { _name = "¬" + (fs._name); _left_boundary = 1.0 - (fs._right_boundary); _peak = 1.0 - (fs._left_boundary); _right_boundary = 1.0 - (fs._peak) }
        raise Return
        __ret
    with
        | Return -> __ret
and intersection (a: FuzzySet) (b: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _name = ((a._name) + " ∩ ") + (b._name); _left_boundary = max2 (a._left_boundary) (b._left_boundary); _peak = min2 (a._right_boundary) (b._right_boundary); _right_boundary = ((a._peak) + (b._peak)) / 2.0 }
        raise Return
        __ret
    with
        | Return -> __ret
and union (a: FuzzySet) (b: FuzzySet) =
    let mutable __ret : FuzzySet = Unchecked.defaultof<FuzzySet>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _name = ((a._name) + " U ") + (b._name); _left_boundary = min2 (a._left_boundary) (b._left_boundary); _peak = max2 (a._right_boundary) (b._right_boundary); _right_boundary = ((a._peak) + (b._peak)) / 2.0 }
        raise Return
        __ret
    with
        | Return -> __ret
and membership (fs: FuzzySet) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fs = fs
    let mutable x = x
    try
        if (x <= (fs._left_boundary)) || (x >= (fs._right_boundary)) then
            __ret <- 0.0
            raise Return
        if ((fs._left_boundary) < x) && (x <= (fs._peak)) then
            __ret <- (x - (fs._left_boundary)) / ((fs._peak) - (fs._left_boundary))
            raise Return
        if ((fs._peak) < x) && (x < (fs._right_boundary)) then
            __ret <- ((fs._right_boundary) - x) / ((fs._right_boundary) - (fs._peak))
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let sheru: FuzzySet = { _name = "Sheru"; _left_boundary = 0.4; _peak = 1.0; _right_boundary = 0.6 }
let siya: FuzzySet = { _name = "Siya"; _left_boundary = 0.5; _peak = 1.0; _right_boundary = 0.7 }
ignore (printfn "%s" (stringify (sheru)))
ignore (printfn "%s" (stringify (siya)))
let sheru_comp: FuzzySet = complement (sheru)
ignore (printfn "%s" (stringify (sheru_comp)))
let inter: FuzzySet = intersection (siya) (sheru)
ignore (printfn "%s" (stringify (inter)))
ignore (printfn "%s" ("Sheru membership 0.5: " + (_str (membership (sheru) (0.5)))))
ignore (printfn "%s" ("Sheru membership 0.6: " + (_str (membership (sheru) (0.6)))))
let uni: FuzzySet = union (siya) (sheru)
ignore (printfn "%s" (stringify (uni)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
