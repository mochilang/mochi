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
let rec max_val (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (int 0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (int i)) > m then
                m <- _idx arr (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_val (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (int 0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (int i)) < m then
                m <- _idx arr (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec counting_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        if (Seq.length (collection)) = 0 then
            __ret <- Array.empty<int>
            raise Return
        let coll_len: int = Seq.length (collection)
        let coll_max: int = max_val (collection)
        let coll_min: int = min_val (collection)
        let counting_arr_length: int = (coll_max + 1) - coll_min
        let mutable counting_arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < counting_arr_length do
            counting_arr <- Array.append counting_arr [|0|]
            i <- i + 1
        i <- 0
        while i < coll_len do
            let number: int = _idx collection (int i)
            counting_arr.[int (number - coll_min)] <- (_idx counting_arr (int (number - coll_min))) + 1
            i <- i + 1
        i <- 1
        while i < counting_arr_length do
            counting_arr.[int i] <- (_idx counting_arr (int i)) + (_idx counting_arr (int (i - 1)))
            i <- i + 1
        let mutable ordered: int array = Array.empty<int>
        i <- 0
        while i < coll_len do
            ordered <- Array.append ordered [|0|]
            i <- i + 1
        let mutable idx: int = coll_len - 1
        while idx >= 0 do
            let number: int = _idx collection (int idx)
            let pos: int = (_idx counting_arr (int (number - coll_min))) - 1
            ordered.[int pos] <- number
            counting_arr.[int (number - coll_min)] <- (_idx counting_arr (int (number - coll_min))) - 1
            idx <- idx - 1
        __ret <- ordered
        raise Return
        __ret
    with
        | Return -> __ret
let ascii_chars: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec chr (code: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable code = code
    try
        if code = 10 then
            __ret <- "\n"
            raise Return
        if code = 13 then
            __ret <- "\r"
            raise Return
        if code = 9 then
            __ret <- "\t"
            raise Return
        if (code >= 32) && (code < 127) then
            __ret <- _substring ascii_chars (code - 32) (code - 31)
            raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "\n" then
            __ret <- 10
            raise Return
        if ch = "\r" then
            __ret <- 13
            raise Return
        if ch = "\t" then
            __ret <- 9
            raise Return
        let mutable i: int = 0
        while i < (String.length (ascii_chars)) do
            if (_substring ascii_chars (i) (i + 1)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec counting_sort_string (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable codes: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (String.length (s)) do
            codes <- Array.append codes [|(ord (_substring s (i) (i + 1)))|]
            i <- i + 1
        let sorted_codes: int array = counting_sort (codes)
        let mutable res: string = ""
        i <- 0
        while i < (Seq.length (sorted_codes)) do
            res <- res + (chr (_idx sorted_codes (int i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let example1: int array = counting_sort (unbox<int array> [|0; 5; 3; 2; 2|])
printfn "%s" (_str (example1))
let example2: int array = counting_sort (Array.empty<int>)
printfn "%s" (_str (example2))
let example3: int array = counting_sort (unbox<int array> [|-2; -5; -45|])
printfn "%s" (_str (example3))
printfn "%s" (counting_sort_string ("thisisthestring"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
