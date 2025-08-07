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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let ASCII_UPPERCASE: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ASCII_LOWERCASE: string = "abcdefghijklmnopqrstuvwxyz"
let NEG_ONE: int = 0 - 1
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- NEG_ONE
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_uppercase (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            let idx: int = index_of (ASCII_LOWERCASE) (ch)
            if idx = NEG_ONE then
                result <- result + ch
            else
                result <- result + (_substring ASCII_UPPERCASE idx (idx + 1))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec gronsfeld (text: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable key = key
    try
        let ascii_len: int = String.length (ASCII_UPPERCASE)
        let key_len: int = String.length (key)
        if key_len = 0 then
            failwith ("integer modulo by zero")
        let upper_text: string = to_uppercase (text)
        let mutable encrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (upper_text)) do
            let ch: string = _substring upper_text i (i + 1)
            let idx: int = index_of (ASCII_UPPERCASE) (ch)
            if idx = NEG_ONE then
                encrypted <- encrypted + ch
            else
                let key_idx: int = ((i % key_len + key_len) % key_len)
                let shift: int = int (_substring key key_idx (key_idx + 1))
                let new_position: int = (((idx + shift) % ascii_len + ascii_len) % ascii_len)
                encrypted <- encrypted + (_substring ASCII_UPPERCASE new_position (new_position + 1))
            i <- i + 1
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (gronsfeld ("hello") ("412"))
printfn "%s" (gronsfeld ("hello") ("123"))
printfn "%s" (gronsfeld ("") ("123"))
printfn "%s" (gronsfeld ("yes, ¥€$ - _!@#%?") ("0"))
printfn "%s" (gronsfeld ("yes, ¥€$ - _!@#%?") ("01"))
printfn "%s" (gronsfeld ("yes, ¥€$ - _!@#%?") ("012"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
