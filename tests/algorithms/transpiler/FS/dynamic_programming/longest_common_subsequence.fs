// Generated 2025-08-07 15:46 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type LcsResult = {
    length: int
    sequence: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec zeros_matrix (rows: int) (cols: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable matrix: int array array = [||]
        let mutable i: int = 0
        while i <= rows do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j <= cols do
                row <- Array.append row [|0|]
                j <- j + 1
            matrix <- Array.append matrix [|row|]
            i <- i + 1
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec longest_common_subsequence (x: string) (y: string) =
    let mutable __ret : LcsResult = Unchecked.defaultof<LcsResult>
    let mutable x = x
    let mutable y = y
    try
        let m: int = String.length (x)
        let n: int = String.length (y)
        let mutable dp: int array array = zeros_matrix (m) (n)
        let mutable i: int = 1
        while i <= m do
            let mutable j: int = 1
            while j <= n do
                if (string (x.[i - 1])) = (string (y.[j - 1])) then
                    dp.[i].[j] <- (_idx (_idx dp (i - 1)) (j - 1)) + 1
                else
                    if (_idx (_idx dp (i - 1)) (j)) > (_idx (_idx dp (i)) (j - 1)) then
                        dp.[i].[j] <- _idx (_idx dp (i - 1)) (j)
                    else
                        dp.[i].[j] <- _idx (_idx dp (i)) (j - 1)
                j <- j + 1
            i <- i + 1
        let mutable seq: string = ""
        let mutable i2: int = m
        let mutable j2: int = n
        while (i2 > 0) && (j2 > 0) do
            if (string (x.[i2 - 1])) = (string (y.[j2 - 1])) then
                seq <- (string (x.[i2 - 1])) + seq
                i2 <- i2 - 1
                j2 <- j2 - 1
            else
                if (_idx (_idx dp (i2 - 1)) (j2)) >= (_idx (_idx dp (i2)) (j2 - 1)) then
                    i2 <- i2 - 1
                else
                    j2 <- j2 - 1
        __ret <- { length = _idx (_idx dp (m)) (n); sequence = seq }
        raise Return
        __ret
    with
        | Return -> __ret
let a: string = "AGGTAB"
let b: string = "GXTXAYB"
let res: LcsResult = longest_common_subsequence (a) (b)
printfn "%s" ((("len = " + (_str (res.length))) + ", sub-sequence = ") + (res.sequence))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
