// Generated 2025-08-08 16:34 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let ascii: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (ascii)) do
            if (_substring ascii (i) (i + 1)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = a
        let mutable ub: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (ua > 0) || (ub > 0) do
            if ((((ua % 2 + 2) % 2)) = 1) && ((((ub % 2 + 2) % 2)) = 1) then
                res <- res + bit
            ua <- int (_floordiv ua 2)
            ub <- int (_floordiv ub 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = a
        let mutable ub: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (ua > 0) || (ub > 0) do
            let abit: int = ((ua % 2 + 2) % 2)
            let bbit: int = ((ub % 2 + 2) % 2)
            if abit <> bbit then
                res <- res + bit
            ua <- int (_floordiv ua 2)
            ub <- int (_floordiv ub 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_not32 (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable ux: int = x
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable count: int = 0
        while count < 32 do
            if (((ux % 2 + 2) % 2)) = 0 then
                res <- res + bit
            ux <- int (_floordiv ux 2)
            bit <- bit * 2
            count <- count + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec elf_hash (data: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable data = data
    try
        let mutable hash_: int = 0
        let mutable i: int = 0
        while i < (String.length (data)) do
            let c: int = ord (_substring data (i) (i + 1))
            hash_ <- (hash_ * 16) + c
            let x: int = bit_and (hash_) (int 4026531840L)
            if x <> 0 then
                hash_ <- bit_xor (hash_) (int (_floordiv x 16777216))
            hash_ <- bit_and (hash_) (bit_not32 (x))
            i <- i + 1
        __ret <- hash_
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (elf_hash ("lorem ipsum")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
