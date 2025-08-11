// Generated 2025-08-11 16:20 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = Array.empty<string>
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (((String.length (sep)) > 0) && ((i + (String.length (sep))) <= (String.length (s)))) && ((_substring s i (i + (String.length (sep)))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length (sep))
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
let rec join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i > 0 then
                res <- res + sep
            res <- res + (_idx xs (int i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec repeat (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < n do
            out <- out + s
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec replace_char (s: string) (old: string) (``new``: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable old = old
    let mutable ``new`` = ``new``
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = _substring s i (i + 1)
            if c = old then
                out <- out + ``new``
            else
                out <- out + c
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (s: string) (sub: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable sub = sub
    try
        if (String.length (sub)) = 0 then
            __ret <- true
            raise Return
        let mutable i: int = 0
        while (i + (String.length (sub))) <= (String.length (s)) do
            if (_substring s i (i + (String.length (sub)))) = sub then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec file_extension (name: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable name = name
    try
        let mutable i: int = (String.length (name)) - 1
        while i >= 0 do
            if (_substring name i (i + 1)) = "." then
                __ret <- _substring name (i) (String.length (name))
                raise Return
            i <- i - 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_extension (name: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable name = name
    try
        let mutable i: int = (String.length (name)) - 1
        while i >= 0 do
            if (_substring name i (i + 1)) = "." then
                __ret <- _substring name (0) (i)
                raise Return
            i <- i - 1
        __ret <- name
        raise Return
        __ret
    with
        | Return -> __ret
let rec title_case (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable cap: bool = true
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = _substring s i (i + 1)
            if c = " " then
                out <- out + c
                cap <- true
            else
                if cap then
                    out <- out + (unbox<string> (c.ToUpper()))
                    cap <- false
                else
                    out <- out + (unbox<string> (c.ToLower()))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_char (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
let rec md_prefix (level: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable level = level
    try
        __ret <- if level = 0 then "\n##" else ((repeat ("  ") (level)) + "*")
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_path (old_path: string) (new_path: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable old_path = old_path
    let mutable new_path = new_path
    try
        let old_parts: string array = split (old_path) ("/")
        let new_parts: string array = split (new_path) ("/")
        let mutable i: int = 0
        while i < (Seq.length (new_parts)) do
            if ((i >= (Seq.length (old_parts))) || ((_idx old_parts (int i)) <> (_idx new_parts (int i)))) && ((_idx new_parts (int i)) <> "") then
                let title: string = title_case (replace_char (_idx new_parts (int i)) ("_") (" "))
                printfn "%s" (((md_prefix (i)) + " ") + title)
            i <- i + 1
        __ret <- new_path
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_strings (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr: string array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable min_idx: int = i
            let mutable j: int = i + 1
            while j < (Seq.length (arr)) do
                if (_idx arr (int j)) < (_idx arr (int min_idx)) then
                    min_idx <- j
                j <- j + 1
            let tmp: string = _idx arr (int i)
            arr.[int i] <- _idx arr (int min_idx)
            arr.[int min_idx] <- tmp
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec good_file_paths (paths: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable paths = paths
    try
        let mutable res: string array = Array.empty<string>
        try
            for p in Seq.map string (paths) do
                try
                    let mutable parts: string array = split (p) ("/")
                    let mutable skip: bool = false
                    let mutable k: int = 0
                    while k < ((Seq.length (parts)) - 1) do
                        let part: string = _idx parts (int k)
                        if (((part = "scripts") || ((_substring part (0) (1)) = ".")) || ((_substring part (0) (1)) = "_")) || (contains (part) ("venv")) then
                            skip <- true
                        k <- k + 1
                    if skip then
                        raise Continue
                    let filename: string = _idx parts (int ((Seq.length (parts)) - 1))
                    if filename = "__init__.py" then
                        raise Continue
                    let ext: string = file_extension (filename)
                    if (ext = ".py") || (ext = ".ipynb") then
                        res <- Array.append res [|p|]
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
let rec print_directory_md (paths: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable paths = paths
    try
        let mutable files: string array = sort_strings (good_file_paths (paths))
        let mutable old_path: string = ""
        let mutable i: int = 0
        while i < (Seq.length (files)) do
            let fp: string = _idx files (int i)
            let mutable parts: string array = split (fp) ("/")
            let filename: string = _idx parts (int ((Seq.length (parts)) - 1))
            let mutable filepath: string = ""
            if (Seq.length (parts)) > 1 then
                filepath <- join (Array.sub parts 0 (((Seq.length (parts)) - 1) - 0)) ("/")
            if filepath <> old_path then
                old_path <- print_path (old_path) (filepath)
            let mutable indent: int = 0
            if (String.length (filepath)) > 0 then
                indent <- (count_char (filepath) ("/")) + 1
            let url: string = replace_char (fp) (" ") ("%20")
            let name: string = title_case (replace_char (remove_extension (filename)) ("_") (" "))
            printfn "%s" ((((((md_prefix (indent)) + " [") + name) + "](") + url) + ")")
            i <- i + 1
        __ret
    with
        | Return -> __ret
let sample: string array = unbox<string array> [|"data_structures/linked_list.py"; "data_structures/binary_tree.py"; "math/number_theory/prime_check.py"; "math/number_theory/greatest_common_divisor.ipynb"|]
print_directory_md (sample)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
