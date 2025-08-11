// Generated 2025-08-11 15:32 +0700

exception Break
exception Continue

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
let rec get_failure_array (pattern: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable pattern = pattern
    try
        let mutable failure: int array = unbox<int array> [|0|]
        let mutable i: int = 0
        let mutable j: int = 1
        try
            while j < (String.length (pattern)) do
                try
                    if (_substring pattern i (i + 1)) = (_substring pattern j (j + 1)) then
                        i <- i + 1
                    else
                        if i > 0 then
                            i <- _idx failure (int (i - 1))
                            raise Continue
                    j <- j + 1
                    failure <- Array.append failure [|i|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- failure
        raise Return
        __ret
    with
        | Return -> __ret
let rec knuth_morris_pratt (text: string) (pattern: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable text = text
    let mutable pattern = pattern
    try
        let mutable failure: int array = get_failure_array (pattern)
        let mutable i: int = 0
        let mutable j: int = 0
        try
            while i < (String.length (text)) do
                try
                    if (_substring pattern j (j + 1)) = (_substring text i (i + 1)) then
                        if j = ((String.length (pattern)) - 1) then
                            __ret <- i - j
                            raise Return
                        j <- j + 1
                    else
                        if j > 0 then
                            j <- _idx failure (int (j - 1))
                            raise Continue
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let text: string = "abcxabcdabxabcdabcdabcy"
let pattern: string = "abcdabcy"
printfn "%d" (knuth_morris_pratt (text) (pattern))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
