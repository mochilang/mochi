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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec starts_with (s: string) (prefix: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable prefix = prefix
    try
        __ret <- if (String.length (s)) < (String.length (prefix)) then false else ((_substring s 0 (String.length (prefix))) = prefix)
        raise Return
        __ret
    with
        | Return -> __ret
let rec all_digits (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c < "0") || (c > "9") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_sri_lankan_phone_number (phone: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable phone = phone
    try
        let mutable p: string = phone
        if starts_with (p) ("+94") then
            p <- _substring p 3 (String.length (p))
        else
            if starts_with (p) ("0094") then
                p <- _substring p 4 (String.length (p))
            else
                if starts_with (p) ("94") then
                    p <- _substring p 2 (String.length (p))
                else
                    if starts_with (p) ("0") then
                        p <- _substring p 1 (String.length (p))
                    else
                        __ret <- false
                        raise Return
        if ((String.length (p)) <> 9) && ((String.length (p)) <> 10) then
            __ret <- false
            raise Return
        if (string (p.[0])) <> "7" then
            __ret <- false
            raise Return
        let second: string = string (p.[1])
        let allowed: string array = unbox<string array> [|"0"; "1"; "2"; "4"; "5"; "6"; "7"; "8"|]
        if not (Seq.contains second allowed) then
            __ret <- false
            raise Return
        let mutable idx: int = 2
        if (String.length (p)) = 10 then
            let sep: string = string (p.[2])
            if (sep <> "-") && (sep <> " ") then
                __ret <- false
                raise Return
            idx <- 3
        if ((String.length (p)) - idx) <> 7 then
            __ret <- false
            raise Return
        let rest: string = _substring p idx (String.length (p))
        __ret <- all_digits (rest)
        raise Return
        __ret
    with
        | Return -> __ret
let phone: string = "0094702343221"
printfn "%s" (_str (is_sri_lankan_phone_number (phone)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
