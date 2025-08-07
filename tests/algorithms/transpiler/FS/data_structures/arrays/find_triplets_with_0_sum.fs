// Generated 2025-08-07 10:31 +0700

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
let rec sort_triplet (a: int) (b: int) (c: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable z: int = c
        if x > y then
            let t: int = x
            x <- y
            y <- t
        if y > z then
            let t: int = y
            y <- z
            z <- t
        if x > y then
            let t: int = x
            x <- y
            y <- t
        __ret <- unbox<int array> [|x; y; z|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_triplet (arr: int array array) (target: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable target = target
    try
        try
            for i in 0 .. ((Seq.length (arr)) - 1) do
                try
                    let item: int array = _idx arr (i)
                    let mutable same: bool = true
                    try
                        for j in 0 .. ((Seq.length (target)) - 1) do
                            try
                                if (_idx item (j)) <> (_idx target (j)) then
                                    same <- false
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if same then
                        __ret <- true
                        raise Return
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_int (arr: int array) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable value = value
    try
        for i in 0 .. ((Seq.length (arr)) - 1) do
            if (_idx arr (i)) = value then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_triplets_with_0_sum (nums: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    try
        let n: int = Seq.length (nums)
        let mutable result: int array array = [||]
        for i in 0 .. (n - 1) do
            for j in i + 1 .. (n - 1) do
                for k in j + 1 .. (n - 1) do
                    let a: int = _idx nums (i)
                    let b: int = _idx nums (j)
                    let c: int = _idx nums (k)
                    if ((a + b) + c) = 0 then
                        let trip: int array = sort_triplet (a) (b) (c)
                        if not (contains_triplet (result) (trip)) then
                            result <- Array.append result [|trip|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_triplets_with_0_sum_hashing (arr: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable arr = arr
    try
        let target_sum: int = 0
        let mutable output: int array array = [||]
        for i in 0 .. ((Seq.length (arr)) - 1) do
            let mutable seen: int array = [||]
            let current_sum: int = target_sum - (_idx arr (i))
            for j in i + 1 .. ((Seq.length (arr)) - 1) do
                let other: int = _idx arr (j)
                let required: int = current_sum - other
                if contains_int (seen) (required) then
                    let trip: int array = sort_triplet (_idx arr (i)) (other) (required)
                    if not (contains_triplet (output) (trip)) then
                        output <- Array.append output [|trip|]
                seen <- Array.append seen [|other|]
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (find_triplets_with_0_sum (unbox<int array> [|-1; 0; 1; 2; -1; -4|])))
printfn "%s" (_str (find_triplets_with_0_sum (Array.empty<int>)))
printfn "%s" (_str (find_triplets_with_0_sum (unbox<int array> [|0; 0; 0|])))
printfn "%s" (_str (find_triplets_with_0_sum (unbox<int array> [|1; 2; 3; 0; -1; -2; -3|])))
printfn "%s" (_str (find_triplets_with_0_sum_hashing (unbox<int array> [|-1; 0; 1; 2; -1; -4|])))
printfn "%s" (_str (find_triplets_with_0_sum_hashing (Array.empty<int>)))
printfn "%s" (_str (find_triplets_with_0_sum_hashing (unbox<int array> [|0; 0; 0|])))
printfn "%s" (_str (find_triplets_with_0_sum_hashing (unbox<int array> [|1; 2; 3; 0; -1; -2; -3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
