// Generated 2025-08-06 20:48 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let alphabet: string = "abcdefghijklmnopqrstuvwxyz"
let rec contains (xs: string array) (x: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length(xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and remove_item (xs: string array) (x: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable res: string array = [||]
        let mutable removed: bool = false
        let mutable i: int = 0
        while i < (Seq.length(xs)) do
            if (not removed) && ((_idx xs (i)) = x) then
                removed <- true
            else
                res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and word_ladder (current: string) (path: string array) (target: string) (words: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable current = current
    let mutable path = path
    let mutable target = target
    let mutable words = words
    try
        if current = target then
            __ret <- path
            raise Return
        let mutable i: int = 0
        while i < (String.length(current)) do
            let mutable j: int = 0
            while j < (String.length(alphabet)) do
                let c: string = _substring alphabet j (j + 1)
                let transformed: string = ((_substring current 0 i) + c) + (_substring current (i + 1) (String.length(current)))
                if contains (words) (transformed) then
                    let new_words: string array = remove_item (words) (transformed)
                    let new_path: string array = Array.append path [|transformed|]
                    let result: string array = word_ladder (transformed) (new_path) (target) (new_words)
                    if (Seq.length(result)) > 0 then
                        __ret <- result
                        raise Return
                j <- j + 1
            i <- i + 1
        __ret <- Array.empty<string>
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let w1: string array = [|"hot"; "dot"; "dog"; "lot"; "log"; "cog"|]
        printfn "%s" (_str (word_ladder ("hit") (unbox<string array> [|"hit"|]) ("cog") (w1)))
        let w2: string array = [|"hot"; "dot"; "dog"; "lot"; "log"|]
        printfn "%s" (_str (word_ladder ("hit") (unbox<string array> [|"hit"|]) ("cog") (w2)))
        let w3: string array = [|"load"; "goad"; "gold"; "lead"; "lord"|]
        printfn "%s" (_str (word_ladder ("lead") (unbox<string array> [|"lead"|]) ("gold") (w3)))
        let w4: string array = [|"came"; "cage"; "code"; "cade"; "gave"|]
        printfn "%s" (_str (word_ladder ("game") (unbox<string array> [|"game"|]) ("code") (w4)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
