// Generated 2025-08-11 15:32 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec min3 (a: int) (b: int) (c: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: int = a
        if b < m then
            m <- b
        if c < m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and edit_distance (source: string) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable source = source
    let mutable target = target
    try
        if (String.length (source)) = 0 then
            __ret <- String.length (target)
            raise Return
        if (String.length (target)) = 0 then
            __ret <- String.length (source)
            raise Return
        let last_source: string = _substring source ((String.length (source)) - 1) (String.length (source))
        let last_target: string = _substring target ((String.length (target)) - 1) (String.length (target))
        let delta: int = if last_source = last_target then 0 else 1
        let delete_cost: int = (edit_distance (_substring source 0 ((String.length (source)) - 1)) (target)) + 1
        let insert_cost: int = (edit_distance (source) (_substring target 0 ((String.length (target)) - 1))) + 1
        let replace_cost: int = (edit_distance (_substring source 0 ((String.length (source)) - 1)) (_substring target 0 ((String.length (target)) - 1))) + delta
        __ret <- min3 (delete_cost) (insert_cost) (replace_cost)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result: int = edit_distance ("ATCGCTG") ("TAGCTAA")
        printfn "%s" (_str (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
