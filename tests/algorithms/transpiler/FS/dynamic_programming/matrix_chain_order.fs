// Generated 2025-08-07 15:46 +0700

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
type MatrixChainResult = {
    matrix: int array array
    solution: int array array
}
let rec make_2d (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_chain_order (arr: int array) =
    let mutable __ret : MatrixChainResult = Unchecked.defaultof<MatrixChainResult>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable m: int array array = make_2d (n)
        let mutable s: int array array = make_2d (n)
        let mutable chain_length: int = 2
        while chain_length < n do
            let mutable a: int = 1
            while a < ((n - chain_length) + 1) do
                let b: int = (a + chain_length) - 1
                m.[a].[b] <- 1000000000
                let mutable c: int = a
                while c < b do
                    let cost: int = ((_idx (_idx m (a)) (c)) + (_idx (_idx m (c + 1)) (b))) + (((_idx arr (a - 1)) * (_idx arr (c))) * (_idx arr (b)))
                    if cost < (_idx (_idx m (a)) (b)) then
                        m.[a].[b] <- cost
                        s.[a].[b] <- c
                    c <- c + 1
                a <- a + 1
            chain_length <- chain_length + 1
        __ret <- { matrix = m; solution = s }
        raise Return
        __ret
    with
        | Return -> __ret
and optimal_parenthesization (s: int array array) (i: int) (j: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable i = i
    let mutable j = j
    try
        if i = j then
            __ret <- "A" + (_str (i))
            raise Return
        else
            let left: string = optimal_parenthesization (s) (i) (_idx (_idx s (i)) (j))
            let right: string = optimal_parenthesization (s) ((_idx (_idx s (i)) (j)) + 1) (j)
            __ret <- ((("( " + left) + " ") + right) + " )"
            raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let arr: int array = [|30; 35; 15; 5; 10; 20; 25|]
        let n: int = Seq.length (arr)
        let mutable res: MatrixChainResult = matrix_chain_order (arr)
        let m: int array array = res.matrix
        let s: int array array = res.solution
        printfn "%s" ("No. of Operation required: " + (_str (_idx (_idx m (1)) (n - 1))))
        let seq: string = optimal_parenthesization (s) (1) (n - 1)
        printfn "%s" (seq)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
