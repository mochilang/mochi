// Generated 2025-08-11 16:20 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
            res <- res + (byte_to_hex (_idx bs (int i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec sha256_hex (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- bytes_to_hex (unbox<int array> (_sha256 (s)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec solution_001 () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let mutable total: int = 0
        let mutable n: int = 0
        while n < 1000 do
            if ((((n % 3 + 3) % 3)) = 0) || ((((n % 5 + 5) % 5)) = 0) then
                total <- total + n
            n <- n + 1
        __ret <- _str (total)
        raise Return
        __ret
    with
        | Return -> __ret
let expected: string = sha256_hex ("233168")
let answer: string = solution_001()
let computed: string = sha256_hex (answer)
if computed = expected then
    printfn "%s" ("Problem 001 passed")
else
    printfn "%s" ((("Problem 001 failed: " + computed) + " != ") + expected)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
