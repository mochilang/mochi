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

let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec find (text: string) (pattern: string) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable text = text
    let mutable pattern = pattern
    let mutable start = start
    try
        let mutable i: int = start
        let limit: int = (String.length (text)) - (String.length (pattern))
        while i <= limit do
            if (_substring text i (i + (String.length (pattern)))) = pattern then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec stock_price (symbol: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable symbol = symbol
    try
        let pages: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("AAPL", "<span data-testid=\"qsp-price\">228.43</span>"); ("AMZN", "<span data-testid=\"qsp-price\">201.85</span>"); ("IBM", "<span data-testid=\"qsp-price\">210.30</span>"); ("GOOG", "<span data-testid=\"qsp-price\">177.86</span>"); ("MSFT", "<span data-testid=\"qsp-price\">414.82</span>"); ("ORCL", "<span data-testid=\"qsp-price\">188.87</span>")]
        if pages.ContainsKey(symbol) then
            let html: string = _dictGet pages ((string (symbol)))
            let marker: string = "<span data-testid=\"qsp-price\">"
            let start_idx: int = find (html) (marker) (0)
            if start_idx <> (-1) then
                let price_start: int = start_idx + (String.length (marker))
                let end_idx: int = find (html) ("</span>") (price_start)
                if end_idx <> (-1) then
                    __ret <- _substring html price_start end_idx
                    raise Return
        __ret <- "No <fin-streamer> tag with the specified data-testid attribute found."
        raise Return
        __ret
    with
        | Return -> __ret
for symbol in [|"AAPL"; "AMZN"; "IBM"; "GOOG"; "MSFT"; "ORCL"|] do
    printfn "%s" ((("Current " + (unbox<string> symbol)) + " stock price is ") + (stock_price (unbox<string> symbol)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
