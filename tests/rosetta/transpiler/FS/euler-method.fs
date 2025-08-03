// Generated 2025-08-03 11:11 +0000

exception Return
let mutable __ret = ()

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and floorf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: int = int x
        __ret <- float y
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < exp do
            r <- r * ``base``
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and fmtF (x: float) (width: int) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable width = width
    let mutable prec = prec
    try
        let factor: float = powf (10.0) (prec)
        let mutable y: float = (floorf ((x * factor) + 0.5)) / factor
        let mutable s: string = string (y)
        let dot = s.IndexOf(".")
        if (int dot) = (0 - 1) then
            s <- s + "."
            let mutable j: int = 0
            while j < prec do
                s <- s + "0"
                j <- j + 1
        else
            let mutable decs = (int ((String.length (s)) - (int dot))) - 1
            while (int decs) < prec do
                s <- s + "0"
                decs <- (int decs) + 1
        while (String.length (s)) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and expf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- float (1.0 / (expf (-x)))
            raise Return
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 20 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and eulerStep (f: float -> float -> float) (x: float) (y: float) (h: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable x = x
    let mutable y = y
    let mutable h = h
    try
        __ret <- float (y + (float (h * (float (f (x) (y))))))
        raise Return
        __ret
    with
        | Return -> __ret
and newCoolingRate (k: float) =
    let mutable __ret : float -> float = Unchecked.defaultof<float -> float>
    let mutable k = k
    try
        __ret <- unbox<float -> float> (        fun (dt: float) -> ((-k) * dt))
        raise Return
        __ret
    with
        | Return -> __ret
and newTempFunc (k: float) (ambient: float) (initial: float) =
    let mutable __ret : float -> float = Unchecked.defaultof<float -> float>
    let mutable k = k
    let mutable ambient = ambient
    let mutable initial = initial
    try
        __ret <- unbox<float -> float> (        fun (t: float) -> (ambient + ((initial - ambient) * (expf ((-k) * t)))))
        raise Return
        __ret
    with
        | Return -> __ret
and newCoolingRateDy (k: float) (ambient: float) =
    let mutable __ret : float -> float -> float = Unchecked.defaultof<float -> float -> float>
    let mutable k = k
    let mutable ambient = ambient
    try
        let cr: float -> float = newCoolingRate (k)
        __ret <- unbox<float -> float -> float> (        fun (_x: float) (obj: float) -> (float (cr (obj - ambient))))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let k: float = 0.07
        let tempRoom: float = 20.0
        let tempObject: float = 100.0
        let fcr: float -> float -> float = newCoolingRateDy (k) (tempRoom)
        let analytic: float -> float = newTempFunc (k) (tempRoom) (tempObject)
        for step in [|2.0; 5.0; 10.0|] do
            printfn "%s" ("Step size = " + (fmtF (float step) (0) (1)))
            printfn "%s" (" Time Euler's Analytic")
            let mutable temp: float = tempObject
            let mutable time: float = 0.0
            while time <= 100.0 do
                let line: string = ((((fmtF (time) (5) (1)) + " ") + (fmtF (temp) (7) (3))) + " ") + (fmtF (analytic (time)) (7) (3))
                printfn "%s" (line)
                temp <- eulerStep (fcr) (time) (temp) (float step)
                time <- float (time + (float step))
            printfn "%s" ("")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
