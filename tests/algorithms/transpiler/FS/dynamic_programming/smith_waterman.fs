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
let rec score_function (source_char: string) (target_char: string) (match_score: int) (mismatch_score: int) (gap_score: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable source_char = source_char
    let mutable target_char = target_char
    let mutable match_score = match_score
    let mutable mismatch_score = mismatch_score
    let mutable gap_score = gap_score
    try
        if (source_char = "-") || (target_char = "-") then
            __ret <- gap_score
            raise Return
        if source_char = target_char then
            __ret <- match_score
            raise Return
        __ret <- mismatch_score
        raise Return
        __ret
    with
        | Return -> __ret
let rec smith_waterman (query: string) (subject: string) (match_score: int) (mismatch_score: int) (gap_score: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable query = query
    let mutable subject = subject
    let mutable match_score = match_score
    let mutable mismatch_score = mismatch_score
    let mutable gap_score = gap_score
    try
        let q: string = query.ToUpper()
        let s: string = subject.ToUpper()
        let m: int = String.length (q)
        let n: int = String.length (s)
        let mutable score: int array array = [||]
        for _ in 0 .. ((m + 1) - 1) do
            let mutable row: int array = [||]
            for _2 in 0 .. ((n + 1) - 1) do
                row <- Array.append row [|0|]
            score <- Array.append score [|row|]
        for i in 1 .. ((m + 1) - 1) do
            for j in 1 .. ((n + 1) - 1) do
                let qc: string = _substring q (i - 1) i
                let sc: string = _substring s (j - 1) j
                let diag: int = (_idx (_idx score (i - 1)) (j - 1)) + (score_function (qc) (sc) (match_score) (mismatch_score) (gap_score))
                let delete: int = (_idx (_idx score (i - 1)) (j)) + gap_score
                let insert: int = (_idx (_idx score (i)) (j - 1)) + gap_score
                let mutable max_val: int = 0
                if diag > max_val then
                    max_val <- diag
                if delete > max_val then
                    max_val <- delete
                if insert > max_val then
                    max_val <- insert
                score.[i].[j] <- max_val
        __ret <- score
        raise Return
        __ret
    with
        | Return -> __ret
let rec traceback (score: int array array) (query: string) (subject: string) (match_score: int) (mismatch_score: int) (gap_score: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable score = score
    let mutable query = query
    let mutable subject = subject
    let mutable match_score = match_score
    let mutable mismatch_score = mismatch_score
    let mutable gap_score = gap_score
    try
        let q: string = query.ToUpper()
        let s: string = subject.ToUpper()
        let mutable max_value: int = 0
        let mutable i_max: int = 0
        let mutable j_max: int = 0
        for i in 0 .. ((Seq.length (score)) - 1) do
            for j in 0 .. ((Seq.length (_idx score (i))) - 1) do
                if (_idx (_idx score (i)) (j)) > max_value then
                    max_value <- _idx (_idx score (i)) (j)
                    i_max <- i
                    j_max <- j
        let mutable i: int = i_max
        let mutable j: int = j_max
        let mutable align1: string = ""
        let mutable align2: string = ""
        let gap_penalty: int = score_function ("-") ("-") (match_score) (mismatch_score) (gap_score)
        if (i = 0) || (j = 0) then
            __ret <- ""
            raise Return
        while (i > 0) && (j > 0) do
            let qc: string = _substring q (i - 1) i
            let sc: string = _substring s (j - 1) j
            if (_idx (_idx score (i)) (j)) = ((_idx (_idx score (i - 1)) (j - 1)) + (score_function (qc) (sc) (match_score) (mismatch_score) (gap_score))) then
                align1 <- qc + align1
                align2 <- sc + align2
                i <- i - 1
                j <- j - 1
            else
                if (_idx (_idx score (i)) (j)) = ((_idx (_idx score (i - 1)) (j)) + gap_penalty) then
                    align1 <- qc + align1
                    align2 <- "-" + align2
                    i <- i - 1
                else
                    align1 <- "-" + align1
                    align2 <- sc + align2
                    j <- j - 1
        __ret <- (align1 + "\n") + align2
        raise Return
        __ret
    with
        | Return -> __ret
let query: string = "HEAGAWGHEE"
let subject: string = "PAWHEAE"
let mutable score: int array array = smith_waterman (query) (subject) (1) (-1) (-2)
printfn "%s" (traceback (score) (query) (subject) (1) (-1) (-2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
