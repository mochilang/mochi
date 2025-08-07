// Generated 2025-08-07 10:31 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let mutable _seed: int = 1
let rec set_seed (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        _seed <- s
        __ret
    with
        | Return -> __ret
let rec randint (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (((_seed % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1))) + a
        raise Return
        __ret
    with
        | Return -> __ret
let ascii_chars: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (ascii_chars)) do
            if (string (ascii_chars.[i])) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (code: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable code = code
    try
        __ret <- if (code < 32) || (code > 126) then "" else (string (ascii_chars.[code - 32]))
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt (text: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int array>>
    let mutable text = text
    try
        let mutable cipher: int array = [||]
        let mutable key: int array = [||]
        let mutable i: int = 0
        while i < (String.length (text)) do
            let p: int = ord (string (text.[i]))
            let k: int = randint (1) (300)
            let c: int = (p + k) * k
            cipher <- Array.append cipher [|c|]
            key <- Array.append key [|k|]
            i <- i + 1
        let mutable res: System.Collections.Generic.IDictionary<string, int array> = _dictCreate []
        res.["cipher"] <- cipher
        res.["key"] <- key
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt (cipher: int array) (key: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cipher = cipher
    let mutable key = key
    try
        let mutable plain: string = ""
        let mutable i: int = 0
        while i < (Seq.length (key)) do
            let p: int = ((_idx cipher (i)) - ((_idx key (i)) * (_idx key (i)))) / (_idx key (i))
            plain <- plain + (chr (p))
            i <- i + 1
        __ret <- plain
        raise Return
        __ret
    with
        | Return -> __ret
set_seed (1)
let res: System.Collections.Generic.IDictionary<string, int array> = encrypt ("Hello")
let mutable cipher: int array = res.[(string ("cipher"))]
let mutable key: int array = res.[(string ("key"))]
printfn "%s" (_repr (cipher))
printfn "%s" (_repr (key))
printfn "%s" (decrypt (cipher) (key))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
