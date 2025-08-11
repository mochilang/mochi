// Generated 2025-08-11 17:23 +0700

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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec match_in_pattern (pat: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable pat = pat
    let mutable ch = ch
    try
        let mutable i: int = (String.length (pat)) - 1
        while i >= 0 do
            if (_substring pat i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i - 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and mismatch_in_text (text: string) (pat: string) (current_pos: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable text = text
    let mutable pat = pat
    let mutable current_pos = current_pos
    try
        let mutable i: int = (String.length (pat)) - 1
        while i >= 0 do
            if (_substring pat i (i + 1)) <> (_substring text (current_pos + i) ((current_pos + i) + 1)) then
                __ret <- current_pos + i
                raise Return
            i <- i - 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and bad_character_heuristic (text: string) (pat: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable text = text
    let mutable pat = pat
    try
        let textLen: int = String.length (text)
        let patLen: int = String.length (pat)
        let mutable positions: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= (textLen - patLen) do
            let mismatch_index: int = mismatch_in_text (text) (pat) (i)
            if mismatch_index < 0 then
                positions <- Array.append positions [|i|]
                i <- i + 1
            else
                let ch: string = _substring text mismatch_index (mismatch_index + 1)
                let match_index: int = match_in_pattern (pat) (ch)
                if match_index < 0 then
                    i <- mismatch_index + 1
                else
                    i <- mismatch_index - match_index
        __ret <- positions
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
