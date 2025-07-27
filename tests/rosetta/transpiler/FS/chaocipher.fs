// Generated 2025-07-27 23:36 +0700

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
let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and rotate (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        __ret <- (s.Substring(n, (String.length s) - n)) + (s.Substring(0, n - 0))
        raise Return
        __ret
    with
        | Return -> __ret
and scrambleLeft (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- (((s.Substring(0, 1 - 0)) + (s.Substring(2, 14 - 2))) + (s.Substring(1, 2 - 1))) + (s.Substring(14, (String.length s) - 14))
        raise Return
        __ret
    with
        | Return -> __ret
and scrambleRight (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- ((((s.Substring(1, 3 - 1)) + (s.Substring(4, 15 - 4))) + (s.Substring(3, 4 - 3))) + (s.Substring(15, (String.length s) - 15))) + (s.Substring(0, 1 - 0))
        raise Return
        __ret
    with
        | Return -> __ret
and chao (text: string) (encode: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable encode = encode
    try
        let mutable left: string = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
        let mutable right: string = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length text) do
            let ch: string = text.Substring(i, (i + 1) - i)
            let mutable idx: int = 0
            if encode then
                idx <- indexOf right ch
                out <- out + (left.Substring(idx, (idx + 1) - idx))
            else
                idx <- indexOf left ch
                out <- out + (right.Substring(idx, (idx + 1) - idx))
            left <- rotate left idx
            right <- rotate right idx
            left <- scrambleLeft left
            right <- scrambleRight right
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let plain: string = "WELLDONEISBETTERTHANWELLSAID"
        let cipher: string = chao plain true
        printfn "%s" plain
        printfn "%s" cipher
        printfn "%s" (chao cipher false)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
