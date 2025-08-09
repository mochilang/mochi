// Generated 2025-08-09 10:14 +0700

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
let rec multiply (matrix_a: int array array) (matrix_b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix_a = matrix_a
    let mutable matrix_b = matrix_b
    try
        let n: int = Seq.length (matrix_a)
        let mutable matrix_c: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                let mutable ``val``: int = 0
                let mutable k: int = 0
                while k < n do
                    ``val`` <- int ((int64 ``val``) + ((int64 (_idx (_idx matrix_a (int i)) (int k))) * (int64 (_idx (_idx matrix_b (int k)) (int j)))))
                    k <- k + 1
                row <- Array.append row [|``val``|]
                j <- j + 1
            matrix_c <- Array.append matrix_c [|row|]
            i <- i + 1
        __ret <- matrix_c
        raise Return
        __ret
    with
        | Return -> __ret
and identity (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and nth_fibonacci_matrix (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 1 then
            __ret <- n
            raise Return
        let mutable res_matrix: int array array = identity (2)
        let mutable fib_matrix: int array array = [|[|1; 1|]; [|1; 0|]|]
        let mutable m: int = n - 1
        while m > 0 do
            if (((m % 2 + 2) % 2)) = 1 then
                res_matrix <- multiply (res_matrix) (fib_matrix)
            fib_matrix <- multiply (fib_matrix) (fib_matrix)
            m <- _floordiv m 2
        __ret <- _idx (_idx res_matrix (int 0)) (int 0)
        raise Return
        __ret
    with
        | Return -> __ret
and nth_fibonacci_bruteforce (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 1 then
            __ret <- n
            raise Return
        let mutable fib0: int = 0
        let mutable fib1: int = 1
        let mutable i: int = 2
        while i <= n do
            let next: int = fib0 + fib1
            fib0 <- fib1
            fib1 <- next
            i <- i + 1
        __ret <- fib1
        raise Return
        __ret
    with
        | Return -> __ret
and parse_number (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable result: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if (ch >= "0") && (ch <= "9") then
                result <- int (((int64 result) * (int64 10)) + (int64 (int ch)))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ordinals: string array = unbox<string array> [|"0th"; "1st"; "2nd"; "3rd"; "10th"; "100th"; "1000th"|]
        let mutable i: int = 0
        while i < (Seq.length (ordinals)) do
            let ordinal: string = _idx ordinals (int i)
            let n: int = parse_number (ordinal)
            let msg: string = (((ordinal + " fibonacci number using matrix exponentiation is ") + (_str (nth_fibonacci_matrix (n)))) + " and using bruteforce is ") + (_str (nth_fibonacci_bruteforce (n)))
            printfn "%s" (msg)
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
