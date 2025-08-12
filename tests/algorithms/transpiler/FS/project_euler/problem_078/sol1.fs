// Generated 2025-08-12 13:41 +0700

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
let rec solution (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        let mutable partitions: int array = unbox<int array> [|1|]
        let mutable i: int = Seq.length (partitions)
        try
            while true do
                try
                    let mutable item: int = 0
                    let mutable j: int = 1
                    try
                        while true do
                            try
                                let sign: int = if (((j % 2 + 2) % 2)) = 0 then (-1) else 1
                                let mutable index: int = _floordiv (((j * j) * 3) - j) 2
                                if index > i then
                                    raise Break
                                item <- item + ((_idx partitions (int (i - index))) * sign)
                                item <- ((item % number + number) % number)
                                index <- index + j
                                if index > i then
                                    raise Break
                                item <- item + ((_idx partitions (int (i - index))) * sign)
                                item <- ((item % number + number) % number)
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if item = 0 then
                        __ret <- i
                        raise Return
                    partitions <- Array.append partitions [|item|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (_str (solution (1))))
        ignore (printfn "%s" (_str (solution (9))))
        ignore (printfn "%s" (_str (solution (1000000))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
