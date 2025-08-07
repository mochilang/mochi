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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec octal_to_hex (octal: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable octal = octal
    try
        let mutable s: string = octal
        if (((String.length (s)) >= 2) && ((string (s.[0])) = "0")) && ((string (s.[1])) = "o") then
            s <- _substring s 2 (String.length (s))
        if (String.length (s)) = 0 then
            failwith ("Empty string was passed to the function")
        let mutable j: int = 0
        while j < (String.length (s)) do
            let c: string = string (s.[j])
            if (((((((c <> "0") && (c <> "1")) && (c <> "2")) && (c <> "3")) && (c <> "4")) && (c <> "5")) && (c <> "6")) && (c <> "7") then
                failwith ("Not a Valid Octal Number")
            j <- j + 1
        let mutable decimal: int = 0
        let mutable k: int = 0
        while k < (String.length (s)) do
            let d: int = unbox<int> (string (s.[k]))
            decimal <- (decimal * 8) + d
            k <- k + 1
        let hex_chars: string = "0123456789ABCDEF"
        if decimal = 0 then
            __ret <- "0x"
            raise Return
        let mutable hex: string = ""
        while decimal > 0 do
            let idx: int = ((decimal % 16 + 16) % 16)
            hex <- (string (hex_chars.[idx])) + hex
            decimal <- decimal / 16
        __ret <- "0x" + hex
        raise Return
        __ret
    with
        | Return -> __ret
let nums: string array = [|"030"; "100"; "247"; "235"; "007"|]
let mutable t: int = 0
while t < (Seq.length (nums)) do
    let num: string = _idx nums (t)
    printfn "%s" (octal_to_hex (num))
    t <- t + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
