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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec cramers_rule_2x2 (eq1: float array) (eq2: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable eq1 = eq1
    let mutable eq2 = eq2
    try
        if ((Seq.length (eq1)) <> 3) || ((Seq.length (eq2)) <> 3) then
            ignore (failwith ("Please enter a valid equation."))
        if ((((_idx eq1 (int 0)) = 0.0) && ((_idx eq1 (int 1)) = 0.0)) && ((_idx eq2 (int 0)) = 0.0)) && ((_idx eq2 (int 1)) = 0.0) then
            ignore (failwith ("Both a & b of two equations can't be zero."))
        let a1: float = _idx eq1 (int 0)
        let b1: float = _idx eq1 (int 1)
        let c1: float = _idx eq1 (int 2)
        let a2: float = _idx eq2 (int 0)
        let b2: float = _idx eq2 (int 1)
        let c2: float = _idx eq2 (int 2)
        let determinant: float = (a1 * b2) - (a2 * b1)
        let determinant_x: float = (c1 * b2) - (c2 * b1)
        let determinant_y: float = (a1 * c2) - (a2 * c1)
        if determinant = 0.0 then
            if (determinant_x = 0.0) && (determinant_y = 0.0) then
                ignore (failwith ("Infinite solutions. (Consistent system)"))
            ignore (failwith ("No solution. (Inconsistent system)"))
        if (determinant_x = 0.0) && (determinant_y = 0.0) then
            __ret <- unbox<float array> [|0.0; 0.0|]
            raise Return
        let x: float = determinant_x / determinant
        let y: float = determinant_y / determinant
        __ret <- unbox<float array> [|x; y|]
        raise Return
        __ret
    with
        | Return -> __ret
and test_cramers_rule_2x2 () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let r1: float array = cramers_rule_2x2 (unbox<float array> [|2.0; 3.0; 0.0|]) (unbox<float array> [|5.0; 1.0; 0.0|])
        if ((_idx r1 (int 0)) <> 0.0) || ((_idx r1 (int 1)) <> 0.0) then
            ignore (failwith ("Test1 failed"))
        let r2: float array = cramers_rule_2x2 (unbox<float array> [|0.0; 4.0; 50.0|]) (unbox<float array> [|2.0; 0.0; 26.0|])
        if ((_idx r2 (int 0)) <> 13.0) || ((_idx r2 (int 1)) <> 12.5) then
            ignore (failwith ("Test2 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_cramers_rule_2x2())
        ignore (printfn "%s" (_repr (cramers_rule_2x2 (unbox<float array> [|11.0; 2.0; 30.0|]) (unbox<float array> [|1.0; 0.0; 4.0|]))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
