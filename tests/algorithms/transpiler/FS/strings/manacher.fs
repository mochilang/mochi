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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec palindromic_string (input_string: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_string = input_string
    try
        let mutable max_length: int = 0
        let mutable new_input_string: string = ""
        let mutable output_string: string = ""
        let n: int = String.length (input_string)
        let mutable i: int = 0
        while i < (n - 1) do
            new_input_string <- (new_input_string + (_substring input_string i (i + 1))) + "|"
            i <- i + 1
        new_input_string <- new_input_string + (_substring input_string (n - 1) n)
        let mutable left: int = 0
        let mutable right: int = 0
        let mutable length: int array = Array.empty<int>
        i <- 0
        let m: int = String.length (new_input_string)
        while i < m do
            length <- Array.append length [|1|]
            i <- i + 1
        let mutable start: int = 0
        let mutable j: int = 0
        while j < m do
            let mutable k: int = 1
            if j <= right then
                let mirror: int = (left + right) - j
                k <- _floordiv (_idx length (int mirror)) 2
                let diff: int = (right - j) + 1
                if diff < k then
                    k <- diff
                if k < 1 then
                    k <- 1
            while (((j - k) >= 0) && ((j + k) < m)) && ((_substring new_input_string (j + k) ((j + k) + 1)) = (_substring new_input_string (j - k) ((j - k) + 1))) do
                k <- k + 1
            length.[int j] <- int (((int64 2) * (int64 k)) - (int64 1))
            if ((j + k) - 1) > right then
                left <- (j - k) + 1
                right <- (j + k) - 1
            if (_idx length (int j)) > max_length then
                max_length <- _idx length (int j)
                start <- j
            j <- j + 1
        let s: string = _substring new_input_string (start - (_floordiv max_length 2)) ((start + (_floordiv max_length 2)) + 1)
        let mutable idx: int = 0
        while idx < (String.length (s)) do
            let ch: string = _substring s idx (idx + 1)
            if ch <> "|" then
                output_string <- output_string + ch
            idx <- idx + 1
        __ret <- output_string
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (palindromic_string ("abbbaba"))
        printfn "%s" (palindromic_string ("ababa"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
