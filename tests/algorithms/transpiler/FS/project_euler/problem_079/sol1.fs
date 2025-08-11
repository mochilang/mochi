// Generated 2025-08-11 16:20 +0700

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
let rec parse_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            value <- int (((int64 value) * (int64 10)) + (int64 (int c)))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec join (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_idx xs (int i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (xs: string array) (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = c then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of (xs: string array) (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = c then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_at (xs: string array) (idx: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec unique_chars (logins: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable logins = logins
    try
        let mutable chars: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (logins)) do
            let login: string = _idx logins (int i)
            let mutable j: int = 0
            while j < (String.length (login)) do
                let c: string = string (login.[j])
                if not (contains (chars) (c)) then
                    chars <- Array.append chars [|c|]
                j <- j + 1
            i <- i + 1
        __ret <- chars
        raise Return
        __ret
    with
        | Return -> __ret
let rec satisfies (permutation: string array) (logins: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable permutation = permutation
    let mutable logins = logins
    try
        let mutable i: int = 0
        while i < (Seq.length (logins)) do
            let login: string = _idx logins (int i)
            let i0: int = index_of (permutation) (string (login.[0]))
            let i1: int = index_of (permutation) (string (login.[1]))
            let i2: int = index_of (permutation) (string (login.[2]))
            if not ((i0 < i1) && (i1 < i2)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec search (chars: string array) (current: string array) (logins: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable chars = chars
    let mutable current = current
    let mutable logins = logins
    try
        if (Seq.length (chars)) = 0 then
            if satisfies (current) (logins) then
                __ret <- join (current)
                raise Return
            __ret <- ""
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (chars)) do
            let c: string = _idx chars (int i)
            let rest: string array = remove_at (chars) (i)
            let next: string array = Array.append current [|c|]
            let mutable res: string = search (rest) (next) (logins)
            if res <> "" then
                __ret <- res
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_secret_passcode (logins: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable logins = logins
    try
        let mutable chars: string array = unique_chars (logins)
        let mutable s: string = search (chars) (Array.empty<string>) (logins)
        if s = "" then
            __ret <- -1
            raise Return
        __ret <- parse_int (s)
        raise Return
        __ret
    with
        | Return -> __ret
let logins1: string array = unbox<string array> [|"135"; "259"; "235"; "189"; "690"; "168"; "120"; "136"; "289"; "589"; "160"; "165"; "580"; "369"; "250"; "280"|]
printfn "%s" (_str (find_secret_passcode (logins1)))
let logins2: string array = unbox<string array> [|"426"; "281"; "061"; "819"; "268"; "406"; "420"; "428"; "209"; "689"; "019"; "421"; "469"; "261"; "681"; "201"|]
printfn "%s" (_str (find_secret_passcode (logins2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
