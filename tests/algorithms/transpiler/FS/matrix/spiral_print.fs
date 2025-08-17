// Generated 2025-08-17 13:19 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let rec is_valid_matrix (matrix: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable matrix = matrix
    try
        if (Seq.length (matrix)) = 0 then
            __ret <- false
            raise Return
        let cols: int = Seq.length (_idx matrix (int 0))
        for row in matrix do
            if (Seq.length (row)) <> cols then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and spiral_traversal (matrix: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable matrix = matrix
    try
        if not (is_valid_matrix (matrix)) then
            __ret <- Array.empty<int>
            raise Return
        let rows: int = Seq.length (matrix)
        let cols: int = Seq.length (_idx matrix (int 0))
        let mutable top: int = 0
        let mutable bottom: int = rows - 1
        let mutable left: int = 0
        let mutable right: int = cols - 1
        let mutable result: int array = Array.empty<int>
        while (left <= right) && (top <= bottom) do
            let mutable i: int = left
            while i <= right do
                result <- Array.append result [|(_idx (_idx matrix (int top)) (int i))|]
                i <- i + 1
            top <- top + 1
            i <- top
            while i <= bottom do
                result <- Array.append result [|(_idx (_idx matrix (int i)) (int right))|]
                i <- i + 1
            right <- right - 1
            if top <= bottom then
                i <- right
                while i >= left do
                    result <- Array.append result [|(_idx (_idx matrix (int bottom)) (int i))|]
                    i <- i - 1
                bottom <- bottom - 1
            if left <= right then
                i <- bottom
                while i >= top do
                    result <- Array.append result [|(_idx (_idx matrix (int i)) (int left))|]
                    i <- i - 1
                left <- left + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and spiral_print_clockwise (matrix: int array array) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable matrix = matrix
    try
        for value in spiral_traversal (matrix) do
            ignore (printfn "%s" (_str (value)))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: int array array = [|[|1; 2; 3; 4|]; [|5; 6; 7; 8|]; [|9; 10; 11; 12|]|]
        ignore (spiral_print_clockwise (a))
        ignore (printfn "%s" (_str (spiral_traversal (a))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
