// Generated 2025-08-11 16:20 +0700

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
let precision: int = 10
let rec lin_search (left: int) (right: int) (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable left = left
    let mutable right = right
    let mutable array = array
    let mutable target = target
    try
        let mutable i: int = left
        while i < right do
            if (_idx array (int i)) = target then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and ite_ternary_search (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable target = target
    try
        let mutable left: int = 0
        let mutable right: int = (Seq.length (array)) - 1
        while left <= right do
            if (right - left) < precision then
                let idx: int = lin_search (left) (right + 1) (array) (target)
                __ret <- idx
                raise Return
            let one_third: int = left + (_floordiv (right - left) 3)
            let two_third: int = right - (_floordiv (right - left) 3)
            if (_idx array (int one_third)) = target then
                __ret <- one_third
                raise Return
            if (_idx array (int two_third)) = target then
                __ret <- two_third
                raise Return
            if target < (_idx array (int one_third)) then
                right <- one_third - 1
            else
                if (_idx array (int two_third)) < target then
                    left <- two_third + 1
                else
                    left <- one_third + 1
                    right <- two_third - 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and rec_ternary_search (left: int) (right: int) (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable left = left
    let mutable right = right
    let mutable array = array
    let mutable target = target
    try
        if left <= right then
            if (right - left) < precision then
                let idx: int = lin_search (left) (right + 1) (array) (target)
                __ret <- idx
                raise Return
            let one_third: int = left + (_floordiv (right - left) 3)
            let two_third: int = right - (_floordiv (right - left) 3)
            if (_idx array (int one_third)) = target then
                __ret <- one_third
                raise Return
            if (_idx array (int two_third)) = target then
                __ret <- two_third
                raise Return
            if target < (_idx array (int one_third)) then
                __ret <- rec_ternary_search (left) (one_third - 1) (array) (target)
                raise Return
            if (_idx array (int two_third)) < target then
                __ret <- rec_ternary_search (two_third + 1) (right) (array) (target)
                raise Return
            __ret <- rec_ternary_search (one_third + 1) (two_third - 1) (array) (target)
            raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let test_list: int array = unbox<int array> [|0; 1; 2; 8; 13; 17; 19; 32; 42|]
        printfn "%s" (_str (ite_ternary_search (test_list) (3)))
        printfn "%s" (_str (ite_ternary_search (test_list) (13)))
        printfn "%s" (_str (rec_ternary_search (0) ((Seq.length (test_list)) - 1) (test_list) (3)))
        printfn "%s" (_str (rec_ternary_search (0) ((Seq.length (test_list)) - 1) (test_list) (13)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
