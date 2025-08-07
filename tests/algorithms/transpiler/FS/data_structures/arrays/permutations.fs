// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec tail (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate_left (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            __ret <- xs
            raise Return
        let mutable res: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        res <- Array.append res [|_idx xs (0)|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec permute_recursive (nums: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            let ``base``: int array array = [||]
            __ret <- Array.append ``base`` [|Array.empty<int>|]
            raise Return
        let mutable result: int array array = [||]
        let mutable current: int array = nums
        let mutable count: int = 0
        while count < (Seq.length (nums)) do
            let n: int = _idx current (0)
            let rest: int array = tail (current)
            let perms: int array array = permute_recursive (rest)
            let mutable j: int = 0
            while j < (Seq.length (perms)) do
                let perm: int array = Array.append (_idx perms (j)) [|n|]
                result <- Array.append result [|perm|]
                j <- j + 1
            current <- rotate_left (current)
            count <- count + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec swap (xs: int array) (i: int) (j: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable i = i
    let mutable j = j
    try
        let mutable res: int array = [||]
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            if k = i then
                res <- Array.append res [|_idx xs (j)|]
            else
                if k = j then
                    res <- Array.append res [|_idx xs (i)|]
                else
                    res <- Array.append res [|_idx xs (k)|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec permute_backtrack_helper (nums: int array) (start: int) (output: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    let mutable start = start
    let mutable output = output
    try
        if start = ((Seq.length (nums)) - 1) then
            __ret <- Array.append output [|nums|]
            raise Return
        let mutable i: int = start
        let mutable res: int array array = output
        while i < (Seq.length (nums)) do
            let swapped: int array = swap (nums) (start) (i)
            res <- permute_backtrack_helper (swapped) (start + 1) (res)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec permute_backtrack (nums: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    try
        let output: int array array = [||]
        __ret <- permute_backtrack_helper (nums) (0) (output)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (permute_recursive (unbox<int array> [|1; 2; 3|])))
printfn "%s" (_str (permute_backtrack (unbox<int array> [|1; 2; 3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
