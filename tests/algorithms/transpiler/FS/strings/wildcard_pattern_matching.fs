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
let rec make_matrix_bool (rows: int) (cols: int) (init: bool) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable rows = rows
    let mutable cols = cols
    let mutable init = init
    try
        let mutable matrix: bool array array = Array.empty<bool array>
        for _ in 0 .. (rows - 1) do
            let mutable row: bool array = Array.empty<bool>
            for _2 in 0 .. (cols - 1) do
                row <- Array.append row [|init|]
            matrix <- Array.append matrix [|row|]
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
and match_pattern (input_string: string) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_string = input_string
    let mutable pattern = pattern
    try
        let len_string: int = (String.length (input_string)) + 1
        let len_pattern: int = (String.length (pattern)) + 1
        let mutable dp: bool array array = make_matrix_bool (len_string) (len_pattern) (false)
        let mutable row0: bool array = _idx dp (int 0)
        row0.[int 0] <- true
        dp.[int 0] <- row0
        let mutable j: int = 1
        while j < len_pattern do
            row0 <- _idx dp (int 0)
            if (_substring pattern (j - 1) j) = "*" then
                row0.[int j] <- _idx row0 (int (j - 2))
            else
                row0.[int j] <- false
            dp.[int 0] <- row0
            j <- j + 1
        let mutable i: int = 1
        while i < len_string do
            let mutable row: bool array = _idx dp (int i)
            let mutable j2: int = 1
            while j2 < len_pattern do
                let s_char: string = _substring input_string (i - 1) i
                let p_char: string = _substring pattern (j2 - 1) j2
                if (s_char = p_char) || (p_char = ".") then
                    row.[int j2] <- _idx (_idx dp (int (i - 1))) (int (j2 - 1))
                else
                    if p_char = "*" then
                        let mutable ``val``: bool = _idx (_idx dp (int i)) (int (j2 - 2))
                        let prev_p: string = _substring pattern (j2 - 2) (j2 - 1)
                        if (not ``val``) && ((prev_p = s_char) || (prev_p = ".")) then
                            ``val`` <- _idx (_idx dp (int (i - 1))) (int j2)
                        row.[int j2] <- ``val``
                    else
                        row.[int j2] <- false
                j2 <- j2 + 1
            dp.[int i] <- row
            i <- i + 1
        __ret <- _idx (_idx dp (int (len_string - 1))) (int (len_pattern - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        if not (match_pattern ("aab") ("c*a*b")) then
            failwith ("case1 failed")
        if match_pattern ("dabc") ("*abc") then
            failwith ("case2 failed")
        if match_pattern ("aaa") ("aa") then
            failwith ("case3 failed")
        if not (match_pattern ("aaa") ("a.a")) then
            failwith ("case4 failed")
        if match_pattern ("aaab") ("aa*") then
            failwith ("case5 failed")
        if not (match_pattern ("aaab") (".*")) then
            failwith ("case6 failed")
        if match_pattern ("a") ("bbbb") then
            failwith ("case7 failed")
        if match_pattern ("") ("bbbb") then
            failwith ("case8 failed")
        if match_pattern ("a") ("") then
            failwith ("case9 failed")
        if not (match_pattern ("") ("")) then
            failwith ("case10 failed")
        printfn "%s" (_str (match_pattern ("aab") ("c*a*b")))
        printfn "%s" (_str (match_pattern ("dabc") ("*abc")))
        printfn "%s" (_str (match_pattern ("aaa") ("aa")))
        printfn "%s" (_str (match_pattern ("aaa") ("a.a")))
        printfn "%s" (_str (match_pattern ("aaab") ("aa*")))
        printfn "%s" (_str (match_pattern ("aaab") (".*")))
        printfn "%s" (_str (match_pattern ("a") ("bbbb")))
        printfn "%s" (_str (match_pattern ("") ("bbbb")))
        printfn "%s" (_str (match_pattern ("a") ("")))
        printfn "%s" (_str (match_pattern ("") ("")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
