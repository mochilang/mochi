// Generated 2025-08-11 17:23 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
let DIGITS: string = "0123456789"
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (index_of (DIGITS) (ch)) >= 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_lower (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let mutable idx: int = index_of (UPPER) (ch)
        if idx >= 0 then
            __ret <- _substring LOWER (idx) (idx + 1)
            raise Return
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
and pad_left (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable res: string = s
        while (String.length (res)) < width do
            res <- "0" + res
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and alphanum_key (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable key: string array = Array.empty<string>
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    if is_digit (string (s.[i])) then
                        let mutable num: string = ""
                        while (i < (String.length (s))) && (is_digit (string (s.[i]))) do
                            num <- num + (string (s.[i]))
                            i <- i + 1
                        let len_str: string = pad_left (_str (String.length (num))) (3)
                        key <- Array.append key [|(("#" + len_str) + num)|]
                    else
                        let mutable seg: string = ""
                        try
                            while i < (String.length (s)) do
                                try
                                    if is_digit (string (s.[i])) then
                                        raise Break
                                    seg <- seg + (to_lower (string (s.[i])))
                                    i <- i + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        key <- Array.append key [|seg|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- key
        raise Return
        __ret
    with
        | Return -> __ret
and compare_keys (a: string array) (b: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        while (i < (Seq.length (a))) && (i < (Seq.length (b))) do
            if (_idx a (int i)) < (_idx b (int i)) then
                __ret <- -1
                raise Return
            if (_idx a (int i)) > (_idx b (int i)) then
                __ret <- 1
                raise Return
            i <- i + 1
        if (Seq.length (a)) < (Seq.length (b)) then
            __ret <- -1
            raise Return
        if (Seq.length (a)) > (Seq.length (b)) then
            __ret <- 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and natural_sort (arr: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable arr = arr
    try
        let mutable res: string array = Array.empty<string>
        let mutable keys: string array array = Array.empty<string array>
        let mutable k: int = 0
        while k < (Seq.length (arr)) do
            res <- Array.append res [|(_idx arr (int k))|]
            keys <- Array.append keys [|(alphanum_key (_idx arr (int k)))|]
            k <- k + 1
        let mutable i: int = 1
        while i < (Seq.length (res)) do
            let current: string = _idx res (int i)
            let current_key: string array = _idx keys (int i)
            let mutable j: int = i - 1
            while (j >= 0) && ((compare_keys (_idx keys (int j)) (current_key)) > 0) do
                res.[int (j + 1)] <- _idx res (int j)
                keys.[int (j + 1)] <- _idx keys (int j)
                j <- j - 1
            res.[int (j + 1)] <- current
            keys.[int (j + 1)] <- current_key
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable example1: string array = unbox<string array> [|"2 ft 7 in"; "1 ft 5 in"; "10 ft 2 in"; "2 ft 11 in"; "7 ft 6 in"|]
printfn "%s" (_str (natural_sort (example1)))
let mutable example2: string array = unbox<string array> [|"Elm11"; "Elm12"; "Elm2"; "elm0"; "elm1"; "elm10"; "elm13"; "elm9"|]
printfn "%s" (_str (natural_sort (example2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
