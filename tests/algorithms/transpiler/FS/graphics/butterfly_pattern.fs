// Generated 2025-08-07 16:27 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec repeat_char (ch: string) (count: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable count = count
    try
        let mutable result: string = ""
        let mutable i: int = 0
        while i < count do
            result <- result + ch
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec butterfly_pattern (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable lines: string array = [||]
        let mutable i: int = 1
        while i < n do
            let left: string = repeat_char ("*") (i)
            let mid: string = repeat_char (" ") ((2 * (n - i)) - 1)
            let right: string = repeat_char ("*") (i)
            lines <- Array.append lines [|((left + mid) + right)|]
            i <- i + 1
        lines <- Array.append lines [|(repeat_char ("*") ((2 * n) - 1))|]
        let mutable j: int = n - 1
        while j > 0 do
            let left: string = repeat_char ("*") (j)
            let mid: string = repeat_char (" ") ((2 * (n - j)) - 1)
            let right: string = repeat_char ("*") (j)
            lines <- Array.append lines [|((left + mid) + right)|]
            j <- j - 1
        let mutable out: string = ""
        let mutable k: int = 0
        while k < (Seq.length (lines)) do
            if k > 0 then
                out <- out + "\n"
            out <- out + (_idx lines (k))
            k <- k + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (butterfly_pattern (3))
printfn "%s" (butterfly_pattern (5))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
