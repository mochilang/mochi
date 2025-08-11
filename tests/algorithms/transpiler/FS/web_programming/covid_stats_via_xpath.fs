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
type CovidData = {
    mutable _cases: int
    mutable _deaths: int
    mutable _recovered: int
}
let rec parse_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let ch: string = _substring s i (i + 1)
                    if ch = "," then
                        i <- i + 1
                        raise Continue
                    value <- int (((int64 value) * (int64 10)) + (int64 (int ch)))
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and find (haystack: string) (needle: string) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable haystack = haystack
    let mutable needle = needle
    let mutable start = start
    try
        let nlen: int = String.length (needle)
        let mutable i: int = start
        try
            while i <= ((String.length (haystack)) - nlen) do
                try
                    let mutable j: int = 0
                    let mutable matched: bool = true
                    try
                        while j < nlen do
                            try
                                if (_substring haystack (i + j) ((i + j) + 1)) <> (_substring needle j (j + 1)) then
                                    matched <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if matched then
                        __ret <- i
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and extract_numbers (html: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable html = html
    try
        let mutable nums: int array = Array.empty<int>
        let mutable pos: int = 0
        let start_tag: string = "<span>"
        let end_tag: string = "</span>"
        try
            while true do
                try
                    let s: int = find (html) (start_tag) (pos)
                    if s = (0 - 1) then
                        raise Break
                    let content_start: int = s + (String.length (start_tag))
                    let e: int = find (html) (end_tag) (content_start)
                    if e = (0 - 1) then
                        raise Break
                    let num_str: string = _substring html content_start e
                    nums <- Array.append nums [|(parse_int (num_str))|]
                    pos <- e + (String.length (end_tag))
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
and covid_stats (html: string) =
    let mutable __ret : CovidData = Unchecked.defaultof<CovidData>
    let mutable html = html
    try
        let mutable nums: int array = extract_numbers (html)
        __ret <- { _cases = _idx nums (int 0); _deaths = _idx nums (int 1); _recovered = _idx nums (int 2) }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let sample_html: string = ("<div class=\"maincounter-number\"><span>123456</span></div>" + "<div class=\"maincounter-number\"><span>7890</span></div>") + "<div class=\"maincounter-number\"><span>101112</span></div>"
        let stats: CovidData = covid_stats (sample_html)
        printfn "%s" ("Total COVID-19 cases in the world: " + (_str (stats._cases)))
        printfn "%s" ("Total deaths due to COVID-19 in the world: " + (_str (stats._deaths)))
        printfn "%s" ("Total COVID-19 patients recovered in the world: " + (_str (stats._recovered)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
