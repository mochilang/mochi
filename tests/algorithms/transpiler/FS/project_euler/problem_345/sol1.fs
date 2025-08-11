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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
let rec parse_row (row_str: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable row_str = row_str
    try
        let mutable nums: int array = Array.empty<int>
        let mutable current: int = 0
        let mutable has_digit: bool = false
        let mutable i: int = 0
        while i < (String.length (row_str)) do
            let ch: string = _substring row_str i (i + 1)
            if ch = " " then
                if has_digit then
                    nums <- Array.append nums [|current|]
                    current <- 0
                    has_digit <- false
            else
                current <- int (((int64 current) * (int64 10)) + (int64 (int ch)))
                has_digit <- true
            i <- i + 1
        if has_digit then
            nums <- Array.append nums [|current|]
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
let rec parse_matrix (matrix_str: string array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix_str = matrix_str
    try
        let mutable matrix: int array array = Array.empty<int array>
        for row_str in Seq.map string (matrix_str) do
            let row: int array = parse_row (row_str)
            matrix <- Array.append matrix [|row|]
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec bitcount (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable count: int = 0
        let mutable y: int = x
        while y > 0 do
            if (((y % 2 + 2) % 2)) = 1 then
                count <- count + 1
            y <- _floordiv y 2
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_powers (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable powers: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable current: int = 1
        while i <= n do
            powers <- Array.append powers [|current|]
            current <- int ((int64 current) * (int64 2))
            i <- i + 1
        __ret <- powers
        raise Return
        __ret
    with
        | Return -> __ret
let rec solution (matrix_str: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable matrix_str = matrix_str
    try
        let arr: int array array = parse_matrix (matrix_str)
        let n: int = Seq.length (arr)
        let mutable powers: int array = build_powers (n)
        let size: int = _idx powers (int n)
        let mutable dp: int array = Array.empty<int>
        let mutable i: int = 0
        while i < size do
            dp <- Array.append dp [|0|]
            i <- i + 1
        let mutable mask: int = 0
        while mask < size do
            let row: int = bitcount (mask)
            if row < n then
                let mutable col: int = 0
                while col < n do
                    if ((((_floordiv mask (_idx powers (int col))) % 2 + 2) % 2)) = 0 then
                        let new_mask: int = mask + (_idx powers (int col))
                        let value: int = (_idx dp (int mask)) + (_idx (_idx arr (int row)) (int col))
                        if value > (_idx dp (int new_mask)) then
                            dp.[int new_mask] <- value
                    col <- col + 1
            mask <- mask + 1
        __ret <- _idx dp (int (size - 1))
        raise Return
        __ret
    with
        | Return -> __ret
let MATRIX_2: string array = unbox<string array> [|"7 53 183 439 863 497 383 563 79 973 287 63 343 169 583"; "627 343 773 959 943 767 473 103 699 303 957 703 583 639 913"; "447 283 463 29 23 487 463 993 119 883 327 493 423 159 743"; "217 623 3 399 853 407 103 983 89 463 290 516 212 462 350"; "960 376 682 962 300 780 486 502 912 800 250 346 172 812 350"; "870 456 192 162 593 473 915 45 989 873 823 965 425 329 803"; "973 965 905 919 133 673 665 235 509 613 673 815 165 992 326"; "322 148 972 962 286 255 941 541 265 323 925 281 601 95 973"; "445 721 11 525 473 65 511 164 138 672 18 428 154 448 848"; "414 456 310 312 798 104 566 520 302 248 694 976 430 392 198"; "184 829 373 181 631 101 969 613 840 740 778 458 284 760 390"; "821 461 843 513 17 901 711 993 293 157 274 94 192 156 574"; "34 124 4 878 450 476 712 914 838 669 875 299 823 329 699"; "815 559 813 459 522 788 168 586 966 232 308 833 251 631 107"; "813 883 451 509 615 77 281 613 459 205 380 274 302 35 805"|]
let result: int = solution (MATRIX_2)
printfn "%s" ("solution() = " + (_str (result)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
