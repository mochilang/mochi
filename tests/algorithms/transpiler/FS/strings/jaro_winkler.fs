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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec min_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a < b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
let rec max_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a > b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
let rec repeat_bool (n: int) (value: bool) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    let mutable value = value
    try
        let mutable res: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|value|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec set_bool (xs: bool array) (idx: int) (value: bool) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable res: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec jaro_winkler (s1: string) (s2: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s1 = s1
    let mutable s2 = s2
    try
        let len1: int = String.length (s1)
        let len2: int = String.length (s2)
        let limit: int = _floordiv (min_int (len1) (len2)) 2
        let mutable match1: bool array = repeat_bool (len1) (false)
        let mutable match2: bool array = repeat_bool (len2) (false)
        let mutable matches: int = 0
        let mutable i: int = 0
        try
            while i < len1 do
                try
                    let start: int = max_int (0) (i - limit)
                    let ``end``: int = min_int ((i + limit) + 1) (len2)
                    let mutable j: int = start
                    try
                        while j < ``end`` do
                            try
                                if (not (_idx match2 (int j))) && ((_substring s1 i (i + 1)) = (_substring s2 j (j + 1))) then
                                    match1 <- set_bool (match1) (i) (true)
                                    match2 <- set_bool (match2) (j) (true)
                                    matches <- matches + 1
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if matches = 0 then
            __ret <- 0.0
            raise Return
        let mutable transpositions: int = 0
        let mutable k: int = 0
        i <- 0
        while i < len1 do
            if _idx match1 (int i) then
                while not (_idx match2 (int k)) do
                    k <- k + 1
                if (_substring s1 i (i + 1)) <> (_substring s2 k (k + 1)) then
                    transpositions <- transpositions + 1
                k <- k + 1
            i <- i + 1
        let m: float = float matches
        let jaro: float = (((m / (float len1)) + (m / (float len2))) + ((m - ((float transpositions) / 2.0)) / m)) / 3.0
        let mutable prefix_len: int = 0
        i <- 0
        try
            while ((i < 4) && (i < len1)) && (i < len2) do
                try
                    if (_substring s1 i (i + 1)) = (_substring s2 i (i + 1)) then
                        prefix_len <- prefix_len + 1
                    else
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- jaro + ((0.1 * (float prefix_len)) * (1.0 - jaro))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (jaro_winkler ("hello") ("world")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
