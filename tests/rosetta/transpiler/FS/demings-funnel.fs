// Generated 2025-07-31 00:10 +0700

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
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable g: float = x
        let mutable i: int = 0
        while i < 20 do
            g <- (g + (x / g)) / 2.0
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let mutable dxs: float array = [|-0.533; 0.27; 0.859; -0.043; -0.205; -0.127; -0.071; 0.275; 1.251; -0.231; -0.401; 0.269; 0.491; 0.951; 1.15; 0.001; -0.382; 0.161; 0.915; 2.08; -2.337; 0.034; -0.126; 0.014; 0.709; 0.129; -1.093; -0.483; -1.193; 0.02; -0.051; 0.047; -0.095; 0.695; 0.34; -0.182; 0.287; 0.213; -0.423; -0.021; -0.134; 1.798; 0.021; -1.099; -0.361; 1.636; -1.134; 1.315; 0.201; 0.034; 0.097; -0.17; 0.054; -0.553; -0.024; -0.181; -0.7; -0.361; -0.789; 0.279; -0.174; -0.009; -0.323; -0.658; 0.348; -0.528; 0.881; 0.021; -0.853; 0.157; 0.648; 1.774; -1.043; 0.051; 0.021; 0.247; -0.31; 0.171; 0.0; 0.106; 0.024; -0.386; 0.962; 0.765; -0.125; -0.289; 0.521; 0.017; 0.281; -0.749; -0.149; -2.436; -0.909; 0.394; -0.113; -0.598; 0.443; -0.521; -0.799; 0.087|]
let mutable dys: float array = [|0.136; 0.717; 0.459; -0.225; 1.392; 0.385; 0.121; -0.395; 0.49; -0.682; -0.065; 0.242; -0.288; 0.658; 0.459; 0.0; 0.426; 0.205; -0.765; -2.188; -0.742; -0.01; 0.089; 0.208; 0.585; 0.633; -0.444; -0.351; -1.087; 0.199; 0.701; 0.096; -0.025; -0.868; 1.051; 0.157; 0.216; 0.162; 0.249; -0.007; 0.009; 0.508; -0.79; 0.723; 0.881; -0.508; 0.393; -0.226; 0.71; 0.038; -0.217; 0.831; 0.48; 0.407; 0.447; -0.295; 1.126; 0.38; 0.549; -0.445; -0.046; 0.428; -0.074; 0.217; -0.822; 0.491; 1.347; -0.141; 1.23; -0.044; 0.079; 0.219; 0.698; 0.275; 0.056; 0.031; 0.421; 0.064; 0.721; 0.104; -0.729; 0.65; -1.103; 0.154; -1.72; 0.051; -0.385; 0.477; 1.537; -0.901; 0.939; -0.411; 0.341; -0.411; 0.106; 0.224; -0.947; -1.424; -0.542; -1.032|]
let rec funnel (fa: float array) (r: float -> float -> float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable fa = fa
    let mutable r = r
    try
        let mutable x: float = 0.0
        let mutable result = [||]
        let mutable i: int = 0
        while i < (Seq.length fa) do
            let f: float = fa.[i]
            result <- Array.append result [|x + f|]
            x <- float (r x f)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and mean (fa: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fa = fa
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length fa) do
            sum <- sum + (fa.[i])
            i <- i + 1
        __ret <- sum / (float (Seq.length fa))
        raise Return
        __ret
    with
        | Return -> __ret
and stdDev (fa: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fa = fa
    try
        let m: float = mean fa
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length fa) do
            let d: float = (fa.[i]) - m
            sum <- sum + (d * d)
            i <- i + 1
        let r: float = sqrtApprox (sum / (float (Seq.length fa)))
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and experiment (label: string) (r: float -> float -> float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable label = label
    let mutable r = r
    try
        let rxs: float array = funnel dxs r
        let rys: float array = funnel dys r
        printfn "%s" (label + "  :      x        y")
        printfn "%s" ((("Mean    :  " + (string (mean rxs))) + ", ") + (string (mean rys)))
        printfn "%s" ((("Std Dev :  " + (string (stdDev rxs))) + ", ") + (string (stdDev rys)))
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        experiment "Rule 1" (unbox<float -> float -> float> (        fun (x: float) (y: float) -> 0.0))
        experiment "Rule 2" (unbox<float -> float -> float> (        fun (x: float) (dz: float) -> (-dz)))
        experiment "Rule 3" (unbox<float -> float -> float> (        fun (z: float) (dz: float) -> (-(z + dz))))
        experiment "Rule 4" (unbox<float -> float -> float> (        fun (z: float) (dz: float) -> (z + dz)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
