// Generated 2025-07-26 04:38 +0700

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
let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 8 do
            let denom: float = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and cosApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n <= 8 do
            let denom: float = float (((2 * n) - 1) * (2 * n))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and atanApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x > 1.0 then
            __ret <- (PI / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        if x < (-1.0) then
            __ret <- ((-PI) / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        __ret <- x / (1.0 + ((0.28 * x) * x))
        raise Return
        __ret
    with
        | Return -> __ret
and atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x > 0.0 then
            __ret <- atanApprox (y / x)
            raise Return
        if x < 0.0 then
            if y >= 0.0 then
                __ret <- (float (atanApprox (y / x))) + PI
                raise Return
            __ret <- (float (atanApprox (y / x))) - PI
            raise Return
        if y > 0.0 then
            __ret <- PI / 2.0
            raise Return
        if y < 0.0 then
            __ret <- (-PI) / 2.0
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and digit (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let digits: string = "0123456789"
        let mutable i: int = 0
        while i < (String.length digits) do
            if (digits.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and parseTwo (s: string) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable idx = idx
    try
        __ret <- ((int (digit (s.Substring(idx, (idx + 1) - idx)))) * 10) + (digit (s.Substring(idx + 1, (idx + 2) - (idx + 1))))
        raise Return
        __ret
    with
        | Return -> __ret
and parseSec (s: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    try
        let h: int = parseTwo s 0
        let m: int = parseTwo s 3
        let sec: int = parseTwo s 6
        let tmp: int = (((h * 60) + m) * 60) + sec
        __ret <- float tmp
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- if n < 10 then ("0" + (string n)) else (string n)
        raise Return
        __ret
    with
        | Return -> __ret
and meanTime (times: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable times = times
    try
        let mutable ssum: float = 0.0
        let mutable csum: float = 0.0
        let mutable i: int = 0
        while i < (int (Array.length times)) do
            let sec: float = parseSec (unbox<string> (times.[i]))
            let ang: float = ((sec * 2.0) * PI) / 86400.0
            ssum <- ssum + (float (sinApprox ang))
            csum <- csum + (float (cosApprox ang))
            i <- i + 1
        let mutable theta: float = atan2Approx ssum csum
        let mutable frac: float = theta / (2.0 * PI)
        while frac < 0.0 do
            frac <- frac + 1.0
        let total: float = frac * 86400.0
        let si: int = int total
        let h: int = int (si / 3600)
        let m: int = int ((((si % 3600 + 3600) % 3600)) / 60)
        let s: int = int (((si % 60 + 60) % 60))
        __ret <- ((((unbox<string> (pad h)) + ":") + (unbox<string> (pad m))) + ":") + (unbox<string> (pad s))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let inputs: string array = [|"23:00:17"; "23:40:20"; "00:12:45"; "00:17:19"|]
        printfn "%A" (meanTime inputs)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
