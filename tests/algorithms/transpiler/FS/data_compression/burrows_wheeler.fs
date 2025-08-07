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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type BWTResult = {
    bwt_string: string
    idx_original_string: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec all_rotations (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let n: int = String.length (s)
        let mutable rotations: string array = [||]
        let mutable i: int = 0
        while i < n do
            let rotation: string = (_substring s i n) + (_substring s 0 i)
            rotations <- Array.append rotations [|rotation|]
            i <- i + 1
        __ret <- rotations
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_strings (arr: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable i: int = 1
        while i < n do
            let mutable key: string = _idx arr (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx arr (j)) > key) do
                arr.[j + 1] <- _idx arr (j)
                j <- j - 1
            arr.[j + 1] <- key
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec join_strings (arr: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            res <- res + (_idx arr (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bwt_transform (s: string) =
    let mutable __ret : BWTResult = Unchecked.defaultof<BWTResult>
    let mutable s = s
    try
        if s = "" then
            failwith ("input string must not be empty")
        let mutable rotations: string array = all_rotations (s)
        rotations <- sort_strings (rotations)
        let mutable last_col: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (rotations)) do
            let word: string = _idx rotations (i)
            last_col <- Array.append last_col [|_substring word ((String.length (word)) - 1) (String.length (word))|]
            i <- i + 1
        let bwt_string: string = join_strings (last_col)
        let idx: int = index_of (rotations) (s)
        __ret <- { bwt_string = bwt_string; idx_original_string = idx }
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of (arr: string array) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable target = target
    try
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) = target then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_bwt (bwt_string: string) (idx_original_string: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bwt_string = bwt_string
    let mutable idx_original_string = idx_original_string
    try
        if bwt_string = "" then
            failwith ("bwt string must not be empty")
        let n: int = String.length (bwt_string)
        if (idx_original_string < 0) || (idx_original_string >= n) then
            failwith ("index out of range")
        let mutable ordered_rotations: string array = [||]
        let mutable i: int = 0
        while i < n do
            ordered_rotations <- Array.append ordered_rotations [|""|]
            i <- i + 1
        let mutable iter: int = 0
        while iter < n do
            let mutable j: int = 0
            while j < n do
                let ch: string = _substring bwt_string j (j + 1)
                ordered_rotations.[j] <- ch + (_idx ordered_rotations (j))
                j <- j + 1
            ordered_rotations <- sort_strings (ordered_rotations)
            iter <- iter + 1
        __ret <- _idx ordered_rotations (idx_original_string)
        raise Return
        __ret
    with
        | Return -> __ret
let s: string = "^BANANA"
let result: BWTResult = bwt_transform (s)
printfn "%s" (result.bwt_string)
printfn "%d" (result.idx_original_string)
printfn "%s" (reverse_bwt (result.bwt_string) (result.idx_original_string))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
