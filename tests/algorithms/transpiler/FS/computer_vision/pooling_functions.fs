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
let rec maxpooling (arr: int array array) (size: int) (stride: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable arr = arr
    let mutable size = size
    let mutable stride = stride
    try
        let n: int = Seq.length (arr)
        if (n = 0) || ((Seq.length (_idx arr (0))) <> n) then
            failwith ("The input array is not a square matrix")
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while (i + size) <= n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while (j + size) <= n do
                let mutable max_val: int = _idx (_idx arr (i)) (j)
                let mutable r: int = i
                while r < (i + size) do
                    let mutable c: int = j
                    while c < (j + size) do
                        let ``val``: int = _idx (_idx arr (r)) (c)
                        if ``val`` > max_val then
                            max_val <- ``val``
                        c <- c + 1
                    r <- r + 1
                row <- Array.append row [|max_val|]
                j <- j + stride
            result <- Array.append result [|row|]
            i <- i + stride
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and avgpooling (arr: int array array) (size: int) (stride: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable arr = arr
    let mutable size = size
    let mutable stride = stride
    try
        let n: int = Seq.length (arr)
        if (n = 0) || ((Seq.length (_idx arr (0))) <> n) then
            failwith ("The input array is not a square matrix")
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while (i + size) <= n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while (j + size) <= n do
                let mutable sum: int = 0
                let mutable r: int = i
                while r < (i + size) do
                    let mutable c: int = j
                    while c < (j + size) do
                        sum <- sum + (_idx (_idx arr (r)) (c))
                        c <- c + 1
                    r <- r + 1
                row <- Array.append row [|sum / (size * size)|]
                j <- j + stride
            result <- Array.append result [|row|]
            i <- i + stride
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and print_matrix (mat: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                line <- line + (_str (_idx (_idx mat (i)) (j)))
                if j < ((Seq.length (_idx mat (i))) - 1) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let arr1: int array array = [|[|1; 2; 3; 4|]; [|5; 6; 7; 8|]; [|9; 10; 11; 12|]; [|13; 14; 15; 16|]|]
        let arr2: int array array = [|[|147; 180; 122|]; [|241; 76; 32|]; [|126; 13; 157|]|]
        print_matrix (maxpooling (arr1) (2) (2))
        print_matrix (maxpooling (arr2) (2) (1))
        print_matrix (avgpooling (arr1) (2) (2))
        print_matrix (avgpooling (arr2) (2) (1))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
