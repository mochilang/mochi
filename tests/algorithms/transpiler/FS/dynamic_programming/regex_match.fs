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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec recursive_match (text: string) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable text = text
    let mutable pattern = pattern
    try
        if (String.length (pattern)) = 0 then
            __ret <- (String.length (text)) = 0
            raise Return
        if (String.length (text)) = 0 then
            if ((String.length (pattern)) >= 2) && ((_substring pattern ((String.length (pattern)) - 1) (String.length (pattern))) = "*") then
                __ret <- recursive_match (text) (_substring pattern 0 ((String.length (pattern)) - 2))
                raise Return
            __ret <- false
            raise Return
        let last_text: string = _substring text ((String.length (text)) - 1) (String.length (text))
        let last_pattern: string = _substring pattern ((String.length (pattern)) - 1) (String.length (pattern))
        if (last_text = last_pattern) || (last_pattern = ".") then
            __ret <- recursive_match (_substring text 0 ((String.length (text)) - 1)) (_substring pattern 0 ((String.length (pattern)) - 1))
            raise Return
        if last_pattern = "*" then
            if recursive_match (_substring text 0 ((String.length (text)) - 1)) (pattern) then
                __ret <- true
                raise Return
            __ret <- recursive_match (text) (_substring pattern 0 ((String.length (pattern)) - 2))
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec dp_match (text: string) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable text = text
    let mutable pattern = pattern
    try
        let m: int = String.length (text)
        let n: int = String.length (pattern)
        let mutable dp: bool array array = [||]
        let mutable i: int = 0
        while i <= m do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j <= n do
                row <- Array.append row [|false|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        dp.[0].[0] <- true
        let mutable j: int = 1
        while j <= n do
            if ((_substring pattern (j - 1) j) = "*") && (j >= 2) then
                if _idx (_idx dp (0)) (j - 2) then
                    dp.[0].[j] <- true
            j <- j + 1
        i <- 1
        while i <= m do
            j <- 1
            while j <= n do
                let p_char: string = _substring pattern (j - 1) j
                let t_char: string = _substring text (i - 1) i
                if (p_char = ".") || (p_char = t_char) then
                    if _idx (_idx dp (i - 1)) (j - 1) then
                        dp.[i].[j] <- true
                else
                    if p_char = "*" then
                        if j >= 2 then
                            if _idx (_idx dp (i)) (j - 2) then
                                dp.[i].[j] <- true
                            let prev_p: string = _substring pattern (j - 2) (j - 1)
                            if (prev_p = ".") || (prev_p = t_char) then
                                if _idx (_idx dp (i - 1)) (j) then
                                    dp.[i].[j] <- true
                    else
                        dp.[i].[j] <- false
                j <- j + 1
            i <- i + 1
        __ret <- _idx (_idx dp (m)) (n)
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_bool (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        if b then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        __ret
    with
        | Return -> __ret
print_bool (recursive_match ("abc") ("a.c"))
print_bool (recursive_match ("abc") ("af*.c"))
print_bool (recursive_match ("abc") ("a.c*"))
print_bool (recursive_match ("abc") ("a.c*d"))
print_bool (recursive_match ("aa") (".*"))
print_bool (dp_match ("abc") ("a.c"))
print_bool (dp_match ("abc") ("af*.c"))
print_bool (dp_match ("abc") ("a.c*"))
print_bool (dp_match ("abc") ("a.c*d"))
print_bool (dp_match ("aa") (".*"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
