// Generated 2025-08-08 20:45 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec inverse_of_matrix (matrix: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    try
        if (((Seq.length (matrix)) = 2) && ((Seq.length (_idx matrix (int 0))) = 2)) && ((Seq.length (_idx matrix (int 1))) = 2) then
            let det: float = ((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 1)) (int 1))) - ((_idx (_idx matrix (int 1)) (int 0)) * (_idx (_idx matrix (int 0)) (int 1)))
            if det = 0.0 then
                printfn "%s" ("This matrix has no inverse.")
                __ret <- Array.empty<float array>
                raise Return
            __ret <- [|[|(_idx (_idx matrix (int 1)) (int 1)) / det; (-(_idx (_idx matrix (int 0)) (int 1))) / det|]; [|(-(_idx (_idx matrix (int 1)) (int 0))) / det; (_idx (_idx matrix (int 0)) (int 0)) / det|]|]
            raise Return
        else
            if ((((Seq.length (matrix)) = 3) && ((Seq.length (_idx matrix (int 0))) = 3)) && ((Seq.length (_idx matrix (int 1))) = 3)) && ((Seq.length (_idx matrix (int 2))) = 3) then
                let det: float = (((((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 1)) (int 1))) * (_idx (_idx matrix (int 2)) (int 2))) + (((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 1)) (int 2))) * (_idx (_idx matrix (int 2)) (int 0)))) + (((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 1)) (int 0))) * (_idx (_idx matrix (int 2)) (int 1)))) - (((((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 1)) (int 1))) * (_idx (_idx matrix (int 2)) (int 0))) + (((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 1)) (int 0))) * (_idx (_idx matrix (int 2)) (int 2)))) + (((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 1)) (int 2))) * (_idx (_idx matrix (int 2)) (int 1))))
                if det = 0.0 then
                    printfn "%s" ("This matrix has no inverse.")
                    __ret <- Array.empty<float array>
                    raise Return
                let mutable cof: float array array = [|[|0.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0|]|]
                cof.[int 0].[int 0] <- ((_idx (_idx matrix (int 1)) (int 1)) * (_idx (_idx matrix (int 2)) (int 2))) - ((_idx (_idx matrix (int 1)) (int 2)) * (_idx (_idx matrix (int 2)) (int 1)))
                cof.[int 0].[int 1] <- -(((_idx (_idx matrix (int 1)) (int 0)) * (_idx (_idx matrix (int 2)) (int 2))) - ((_idx (_idx matrix (int 1)) (int 2)) * (_idx (_idx matrix (int 2)) (int 0))))
                cof.[int 0].[int 2] <- ((_idx (_idx matrix (int 1)) (int 0)) * (_idx (_idx matrix (int 2)) (int 1))) - ((_idx (_idx matrix (int 1)) (int 1)) * (_idx (_idx matrix (int 2)) (int 0)))
                cof.[int 1].[int 0] <- -(((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 2)) (int 2))) - ((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 2)) (int 1))))
                cof.[int 1].[int 1] <- ((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 2)) (int 2))) - ((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 2)) (int 0)))
                cof.[int 1].[int 2] <- -(((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 2)) (int 1))) - ((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 2)) (int 0))))
                cof.[int 2].[int 0] <- ((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 1)) (int 2))) - ((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 1)) (int 1)))
                cof.[int 2].[int 1] <- -(((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 1)) (int 2))) - ((_idx (_idx matrix (int 0)) (int 2)) * (_idx (_idx matrix (int 1)) (int 0))))
                cof.[int 2].[int 2] <- ((_idx (_idx matrix (int 0)) (int 0)) * (_idx (_idx matrix (int 1)) (int 1))) - ((_idx (_idx matrix (int 0)) (int 1)) * (_idx (_idx matrix (int 1)) (int 0)))
                let mutable inv: float array array = [|[|0.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0|]|]
                let mutable i: int = 0
                while i < 3 do
                    let mutable j: int = 0
                    while j < 3 do
                        inv.[int i].[int j] <- (_idx (_idx cof (int j)) (int i)) / det
                        j <- j + 1
                    i <- i + 1
                __ret <- inv
                raise Return
        printfn "%s" ("Please provide a matrix of size 2x2 or 3x3.")
        __ret <- Array.empty<float array>
        raise Return
        __ret
    with
        | Return -> __ret
let mutable m2: float array array = [|[|2.0; 5.0|]; [|2.0; 0.0|]|]
printfn "%s" (_repr (inverse_of_matrix (m2)))
let mutable m3: float array array = [|[|2.0; 5.0; 7.0|]; [|2.0; 0.0; 1.0|]; [|1.0; 2.0; 3.0|]|]
printfn "%s" (_repr (inverse_of_matrix (m3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
