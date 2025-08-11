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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let DIGITS: string = "0123456789"
let rec is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (DIGITS)) do
            if (string (DIGITS.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_substring (haystack: string) (needle: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable haystack = haystack
    let mutable needle = needle
    try
        let mutable i: int = 0
        try
            while i <= ((String.length (haystack)) - (String.length (needle))) do
                try
                    let mutable j: int = 0
                    try
                        while j < (String.length (needle)) do
                            try
                                if (string (haystack.[i + j])) <> (string (needle.[j])) then
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if j = (String.length (needle)) then
                        __ret <- i
                        raise Return
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
let rec extract_citation (html: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable html = html
    try
        let marker: string = "Cited by "
        let idx: int = find_substring (html) (marker)
        if idx < 0 then
            __ret <- ""
            raise Return
        let mutable pos: int = idx + (String.length (marker))
        let mutable result: string = ""
        try
            while pos < (String.length (html)) do
                try
                    let ch: string = string (html.[pos])
                    if not (is_digit (ch)) then
                        raise Break
                    result <- result + ch
                    pos <- pos + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_citation (base_url: string) (params: System.Collections.Generic.IDictionary<string, string>) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable base_url = base_url
    let mutable params = params
    try
        let html: string = "<div class=\"gs_ri\"><div class=\"gs_fl\"><a>Cited by 123</a></div></div>"
        __ret <- extract_citation (html)
        raise Return
        __ret
    with
        | Return -> __ret
if (unbox<string> __name__) = "__main__" then
    let params: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("title", "Precisely geometry controlled microsupercapacitors for ultrahigh areal capacitance, volumetric capacitance, and energy density"); ("journal", "Chem. Mater."); ("volume", "30"); ("pages", "3979-3990"); ("year", "2018"); ("hl", "en")]
    printfn "%s" (get_citation ("https://scholar.google.com/scholar_lookup") (params))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
