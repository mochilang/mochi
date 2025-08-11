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
type Case = {
    mutable _text: string
    mutable _expected: bool
}
let rec reverse (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = (String.length (s)) - 1
        while i >= 0 do
            res <- res + (string (s.[i]))
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and is_palindrome (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable start_i: int = 0
        let mutable end_i: int = (String.length (s)) - 1
        while start_i < end_i do
            if (string (s.[start_i])) = (string (s.[end_i])) then
                start_i <- start_i + 1
                end_i <- end_i - 1
            else
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and is_palindrome_traversal (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let ``end``: int = _floordiv (String.length (s)) 2
        let n: int = String.length (s)
        let mutable i: int = 0
        while i < ``end`` do
            if (string (s.[i])) <> (string (s.[(n - i) - 1])) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and is_palindrome_recursive (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if (String.length (s)) <= 1 then
            __ret <- true
            raise Return
        if (string (s.[0])) = (string (s.[(String.length (s)) - 1])) then
            __ret <- is_palindrome_recursive (_substring s (1) ((String.length (s)) - 1))
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and is_palindrome_slice (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- s = (reverse (s))
        raise Return
        __ret
    with
        | Return -> __ret
let test_data: Case array = unbox<Case array> [|{ _text = "MALAYALAM"; _expected = true }; { _text = "String"; _expected = false }; { _text = "rotor"; _expected = true }; { _text = "level"; _expected = true }; { _text = "A"; _expected = true }; { _text = "BB"; _expected = true }; { _text = "ABC"; _expected = false }; { _text = "amanaplanacanalpanama"; _expected = true }|]
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for t in test_data do
            let s: string = t._text
            let _expected: bool = t._expected
            let r1: bool = is_palindrome (s)
            let r2: bool = is_palindrome_traversal (s)
            let r3: bool = is_palindrome_recursive (s)
            let r4: bool = is_palindrome_slice (s)
            if (((r1 <> _expected) || (r2 <> _expected)) || (r3 <> _expected)) || (r4 <> _expected) then
                failwith ("algorithm mismatch")
            printfn "%s" ((s + " ") + (_str (_expected)))
        printfn "%s" ("a man a plan a canal panama")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
