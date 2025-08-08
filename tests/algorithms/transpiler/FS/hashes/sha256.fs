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
let _sha256 (s:string) : int array =
    use sha = System.Security.Cryptography.SHA256.Create()
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    sha.ComputeHash(bytes) |> Array.map int

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Security.Cryptography

open System.Text

let HEX: string = "0123456789abcdef"
let rec byte_to_hex (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        let hi: int = _floordiv b 16
        let lo: int = ((b % 16 + 16) % 16)
        __ret <- (string (HEX.[hi])) + (string (HEX.[lo]))
        raise Return
        __ret
    with
        | Return -> __ret
let rec bytes_to_hex (bs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bs = bs
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (bs)) do
            res <- res + (byte_to_hex (_idx bs (i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (bytes_to_hex (unbox<int array> (_sha256 ("Python"))))
printfn "%s" (bytes_to_hex (unbox<int array> (_sha256 ("hello world"))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
