// Generated 2025-08-08 17:07 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let PUNCT: string = "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~"
let rec to_lowercase (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let c: string = string (s.[i])
                    let mutable j: int = 0
                    let mutable found: bool = false
                    try
                        while j < (String.length (UPPER)) do
                            try
                                if c = (string (UPPER.[j])) then
                                    res <- res + (string (LOWER.[j]))
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        res <- res + c
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_punct (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (PUNCT)) do
            if c = (string (PUNCT.[i])) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec clean_text (text: string) (keep_newlines: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable keep_newlines = keep_newlines
    try
        let lower: string = to_lowercase (text)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (lower)) do
            let ch: string = string (lower.[i])
            if is_punct (ch) then ()
            else
                if ch = "\n" then
                    if keep_newlines then
                        res <- res + "\n"
                else
                    res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            if ch = sep then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (s: string) (sub: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable sub = sub
    try
        let n: int = String.length (s)
        let m: int = String.length (sub)
        if m = 0 then
            __ret <- true
            raise Return
        let mutable i: int = 0
        try
            while i <= (n - m) do
                try
                    let mutable j: int = 0
                    let mutable is_match: bool = true
                    try
                        while j < m do
                            try
                                if (string (s.[i + j])) <> (string (sub.[j])) then
                                    is_match <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_match then
                        __ret <- true
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec round3 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (floor ((x * 1000.0) + 0.5)) / 1000.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable k: int = 1
        while k <= 99 do
            sum <- sum + (term / (float k))
            term <- (term * t) * t
            k <- k + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec log10 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (ln (x)) / (ln (10.0))
        raise Return
        __ret
    with
        | Return -> __ret
let rec term_frequency (term: string) (document: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable term = term
    let mutable document = document
    try
        let clean: string = clean_text (document) (false)
        let tokens: string array = split (clean) (" ")
        let t: string = to_lowercase (term)
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (Seq.length (tokens)) do
            if ((_idx tokens (i)) <> "") && ((_idx tokens (i)) = t) then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let rec document_frequency (term: string) (corpus: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable term = term
    let mutable corpus = corpus
    try
        let clean: string = clean_text (corpus) (true)
        let docs: string array = split (clean) ("\n")
        let t: string = to_lowercase (term)
        let mutable matches: int = 0
        let mutable i: int = 0
        while i < (Seq.length (docs)) do
            if contains (_idx docs (i)) (t) then
                matches <- matches + 1
            i <- i + 1
        __ret <- unbox<int array> [|matches; Seq.length (docs)|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec inverse_document_frequency (df: int) (n: int) (smoothing: bool) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable df = df
    let mutable n = n
    let mutable smoothing = smoothing
    try
        if smoothing then
            if n = 0 then
                failwith ("log10(0) is undefined.")
            let ratio: float = (float n) / (1.0 + (float df))
            let l: float = log10 (ratio)
            let result: float = round3 (1.0 + l)
            printfn "%g" (result)
            __ret <- result
            raise Return
        if df = 0 then
            failwith ("df must be > 0")
        if n = 0 then
            failwith ("log10(0) is undefined.")
        let ratio: float = (float n) / (float df)
        let l: float = log10 (ratio)
        let result: float = round3 (l)
        printfn "%g" (result)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec tf_idf (tf: int) (idf: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable tf = tf
    let mutable idf = idf
    try
        let prod: float = (float tf) * idf
        let result: float = round3 (prod)
        printfn "%g" (result)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (term_frequency ("to") ("To be, or not to be"))
let corpus: string = "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus."
printfn "%s" (_str (document_frequency ("first") (corpus)))
let idf_val: float = inverse_document_frequency (1) (3) (false)
tf_idf (2) (idf_val)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
