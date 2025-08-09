// Generated 2025-08-09 15:58 +0700

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
let rec allConstruct (target: string) (wordBank: string array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable target = target
    let mutable wordBank = wordBank
    try
        let tableSize: int = (String.length (target)) + 1
        let mutable table: string array array array = Array.empty<string array array>
        let mutable idx: int = 0
        while idx < tableSize do
            let mutable empty: string array array = Array.empty<string array>
            table <- Array.append table [|empty|]
            idx <- idx + 1
        let mutable ``base``: string array = Array.empty<string>
        table.[int 0] <- [|``base``|]
        let mutable i: int = 0
        while i < tableSize do
            if (Seq.length (_idx table (int i))) <> 0 then
                let mutable w: int = 0
                while w < (Seq.length (wordBank)) do
                    let word: string = _idx wordBank (int w)
                    let wordLen: int = String.length (word)
                    if (_substring target (i) (i + wordLen)) = word then
                        let mutable k: int = 0
                        while k < (Seq.length (_idx table (int i))) do
                            let way: string array = _idx (_idx table (int i)) (int k)
                            let mutable combination: string array = Array.empty<string>
                            let mutable m: int = 0
                            while m < (Seq.length (way)) do
                                combination <- Array.append combination [|(_idx way (int m))|]
                                m <- m + 1
                            combination <- Array.append combination [|word|]
                            let nextIndex: int = i + wordLen
                            table.[int nextIndex] <- Array.append (_idx table (int nextIndex)) [|combination|]
                            k <- k + 1
                    w <- w + 1
            i <- i + 1
        __ret <- _idx table (int (String.length (target)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (allConstruct ("jwajalapa") (unbox<string array> [|"jwa"; "j"; "w"; "a"; "la"; "lapa"|])))
printfn "%s" (_str (allConstruct ("rajamati") (unbox<string array> [|"s"; "raj"; "amat"; "raja"; "ma"; "i"; "t"|])))
printfn "%s" (_str (allConstruct ("hexagonosaurus") (unbox<string array> [|"h"; "ex"; "hex"; "ag"; "ago"; "ru"; "auru"; "rus"; "go"; "no"; "o"; "s"|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
