// Generated 2025-08-07 15:46 +0700

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
let rec find_min (numbers: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable numbers = numbers
    try
        let n: int = Seq.length (numbers)
        let mutable s: int = 0
        let mutable idx: int = 0
        while idx < n do
            s <- s + (_idx numbers (idx))
            idx <- idx + 1
        let mutable dp: bool array array = [||]
        let mutable i: int = 0
        while i <= n do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j <= s do
                row <- Array.append row [|false|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        i <- 0
        while i <= n do
            dp.[i].[0] <- true
            i <- i + 1
        let mutable j: int = 1
        while j <= s do
            dp.[0].[j] <- false
            j <- j + 1
        i <- 1
        while i <= n do
            j <- 1
            while j <= s do
                dp.[i].[j] <- _idx (_idx dp (i - 1)) (j)
                if (_idx numbers (i - 1)) <= j then
                    if _idx (_idx dp (i - 1)) (j - (_idx numbers (i - 1))) then
                        dp.[i].[j] <- true
                j <- j + 1
            i <- i + 1
        let mutable diff: int = 0
        j <- s / 2
        try
            while j >= 0 do
                try
                    if _idx (_idx dp (n)) (j) then
                        diff <- s - (2 * j)
                        raise Break
                    j <- j - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- diff
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (find_min (unbox<int array> [|1; 2; 3; 4; 5|])))
printfn "%s" (_str (find_min (unbox<int array> [|5; 5; 5; 5; 5|])))
printfn "%s" (_str (find_min (unbox<int array> [|5; 5; 5; 5|])))
printfn "%s" (_str (find_min (unbox<int array> [|3|])))
printfn "%s" (_str (find_min (Array.empty<int>)))
printfn "%s" (_str (find_min (unbox<int array> [|1; 2; 3; 4|])))
printfn "%s" (_str (find_min (unbox<int array> [|0; 0; 0; 0|])))
printfn "%s" (_str (find_min (unbox<int array> [|-1; -5; 5; 1|])))
printfn "%s" (_str (find_min (unbox<int array> [|9; 9; 9; 9; 9|])))
printfn "%s" (_str (find_min (unbox<int array> [|1; 5; 10; 3|])))
printfn "%s" (_str (find_min (unbox<int array> [|-1; 0; 1|])))
printfn "%s" (_str (find_min (unbox<int array> [|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
