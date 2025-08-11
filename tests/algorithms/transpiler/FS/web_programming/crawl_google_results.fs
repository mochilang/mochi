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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let rec index_of_from (s: string) (sub: string) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    let mutable start = start
    try
        let mutable i: int = start
        let max: int = (String.length (s)) - (String.length (sub))
        while i <= max do
            if (_substring s (i) (i + (String.length (sub)))) = sub then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and extract_links (html: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, string> array = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, string> array>
    let mutable html = html
    try
        let mutable res: System.Collections.Generic.IDictionary<string, string> array = Array.empty<System.Collections.Generic.IDictionary<string, string>>
        let mutable i: int = 0
        try
            while true do
                try
                    let tag_start: int = index_of_from (html) ("<a class=\"eZt8xd\"") (i)
                    if tag_start = (-1) then
                        raise Break
                    let mutable href_start: int = index_of_from (html) ("href=\"") (tag_start)
                    if href_start = (-1) then
                        raise Break
                    href_start <- href_start + (String.length ("href=\""))
                    let href_end: int = index_of_from (html) ("\"") (href_start)
                    if href_end = (-1) then
                        raise Break
                    let href: string = _substring html (href_start) (href_end)
                    let text_start: int = (index_of_from (html) (">") (href_end)) + 1
                    let text_end: int = index_of_from (html) ("</a>") (text_start)
                    if text_end = (-1) then
                        raise Break
                    let text: string = _substring html (text_start) (text_end)
                    let link: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("href", href); ("text", text)]
                    res <- Array.append res [|link|]
                    i <- text_end + (String.length ("</a>"))
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
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let html: string = ("<div><a class=\"eZt8xd\" href=\"/url?q=http://example1.com\">Example1</a>" + "<a class=\"eZt8xd\" href=\"/maps\">Maps</a>") + "<a class=\"eZt8xd\" href=\"/url?q=http://example2.com\">Example2</a></div>"
        let links: System.Collections.Generic.IDictionary<string, string> array = extract_links (html)
        printfn "%s" (_str (Seq.length (links)))
        let mutable i: int = 0
        while (i < (Seq.length (links))) && (i < 5) do
            let link: System.Collections.Generic.IDictionary<string, string> = _idx links (int i)
            let href: string = _dictGet link ((string ("href")))
            let text: string = _dictGet link ((string ("text")))
            if text = "Maps" then
                printfn "%s" (href)
            else
                printfn "%s" ("https://google.com" + href)
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
