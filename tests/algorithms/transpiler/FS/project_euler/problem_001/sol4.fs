// Generated 2025-08-09 23:14 +0700

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
let rec contains (xs: int array) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable zmulti: int array = Array.empty<int>
        let mutable xmulti: int array = Array.empty<int>
        let mutable temp: int = 1
        try
            while true do
                try
                    let result: int64 = (int64 3) * (int64 temp)
                    if result < (int64 n) then
                        zmulti <- Array.append zmulti [|int (result)|]
                        temp <- temp + 1
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        temp <- 1
        try
            while true do
                try
                    let result: int64 = (int64 5) * (int64 temp)
                    if result < (int64 n) then
                        xmulti <- Array.append xmulti [|int (result)|]
                        temp <- temp + 1
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable collection: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (zmulti)) do
            let v: int = _idx zmulti (int i)
            if not (contains (collection) (v)) then
                collection <- Array.append collection [|v|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (xmulti)) do
            let v: int = _idx xmulti (int i)
            if not (contains (collection) (v)) then
                collection <- Array.append collection [|v|]
            i <- i + 1
        let mutable total: int = 0
        i <- 0
        while i < (Seq.length (collection)) do
            total <- total + (_idx collection (int i))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and test_solution () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (solution (3)) <> 0 then
            failwith ("solution(3) failed")
        if (solution (4)) <> 3 then
            failwith ("solution(4) failed")
        if (solution (10)) <> 23 then
            failwith ("solution(10) failed")
        if (solution (600)) <> 83700 then
            failwith ("solution(600) failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_solution()
        printfn "%s" ("solution() = " + (_str (solution (1000))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
