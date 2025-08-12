// Generated 2025-08-12 07:47 +0700

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
let rec remove_at (xs: int array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and kth_permutation (k: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable k = k
    let mutable n = n
    try
        if n <= 0 then
            failwith ("n must be positive")
        let mutable factorials: int array = unbox<int array> [|1|]
        let mutable i: int = 2
        while i < n do
            factorials <- Array.append factorials [|int ((int64 (_idx factorials (int ((Seq.length (factorials)) - 1)))) * (int64 i))|]
            i <- i + 1
        let total: int64 = (int64 (_idx factorials (int ((Seq.length (factorials)) - 1)))) * (int64 n)
        if (k < 0) || ((int64 k) >= total) then
            failwith ("k out of bounds")
        let mutable elements: int array = Array.empty<int>
        let mutable e: int = 0
        while e < n do
            elements <- Array.append elements [|e|]
            e <- e + 1
        let mutable permutation: int array = Array.empty<int>
        let mutable idx: int = (Seq.length (factorials)) - 1
        while idx >= 0 do
            let factorial: int = _idx factorials (int idx)
            let number: int = _floordiv k factorial
            k <- ((k % factorial + factorial) % factorial)
            permutation <- Array.append permutation [|(_idx elements (int number))|]
            elements <- remove_at (elements) (number)
            idx <- idx - 1
        permutation <- Array.append permutation [|(_idx elements (int 0))|]
        __ret <- permutation
        raise Return
        __ret
    with
        | Return -> __ret
and list_equal (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (int i)) <> (_idx b (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            __ret <- "[]"
            raise Return
        let mutable s: string = "[" + (_str (_idx xs (int 0)))
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            s <- (s + ", ") + (_str (_idx xs (int i)))
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and test_kth_permutation () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let expected1: int array = unbox<int array> [|0; 1; 2; 3; 4|]
        let res1: int array = kth_permutation (0) (5)
        if not (list_equal (res1) (expected1)) then
            failwith ("test case 1 failed")
        let expected2: int array = unbox<int array> [|1; 3; 0; 2|]
        let res2: int array = kth_permutation (10) (4)
        if not (list_equal (res2) (expected2)) then
            failwith ("test case 2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_kth_permutation()
        let mutable res: int array = kth_permutation (10) (4)
        printfn "%s" (list_to_string (res))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
