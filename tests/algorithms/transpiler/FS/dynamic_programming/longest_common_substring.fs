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
let rec longest_common_substring (text1: string) (text2: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text1 = text1
    let mutable text2 = text2
    try
        if ((String.length (text1)) = 0) || ((String.length (text2)) = 0) then
            __ret <- ""
            raise Return
        let m: int = String.length (text1)
        let n: int = String.length (text2)
        let mutable dp: int array array = [||]
        let mutable i: int = 0
        while i < (m + 1) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (n + 1) do
                row <- Array.append row [|0|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        let mutable end_pos: int = 0
        let mutable max_len: int = 0
        let mutable ii: int = 1
        while ii <= m do
            let mutable jj: int = 1
            while jj <= n do
                if (_substring text1 (ii - 1) ii) = (_substring text2 (jj - 1) jj) then
                    dp.[ii].[jj] <- 1 + (_idx (_idx dp (ii - 1)) (jj - 1))
                    if (_idx (_idx dp (ii)) (jj)) > max_len then
                        max_len <- _idx (_idx dp (ii)) (jj)
                        end_pos <- ii
                jj <- jj + 1
            ii <- ii + 1
        __ret <- _substring text1 (end_pos - max_len) end_pos
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (longest_common_substring ("abcdef") ("xabded"))
printfn "%s" ("\n")
printfn "%s" (longest_common_substring ("zxabcdezy") ("yzabcdezx"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
