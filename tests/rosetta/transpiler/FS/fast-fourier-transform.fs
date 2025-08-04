// Generated 2025-08-04 20:44 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type Complex = {
    re: float
    im: float
}
let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 10 do
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
        while n <= 10 do
            let denom: float = float (((2 * n) - 1) * (2 * n))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and cis (x: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable x = x
    try
        __ret <- { re = cosApprox (x); im = sinApprox (x) }
        raise Return
        __ret
    with
        | Return -> __ret
and add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = (a.re) + (b.re); im = (a.im) + (b.im) }
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = (a.re) - (b.re); im = (a.im) - (b.im) }
        raise Return
        __ret
    with
        | Return -> __ret
and mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = ((a.re) * (b.re)) - ((a.im) * (b.im)); im = ((a.re) * (b.im)) + ((a.im) * (b.re)) }
        raise Return
        __ret
    with
        | Return -> __ret
and ditfft2Rec (x: float array) (y: Complex array) (offX: int) (offY: int) (n: int) (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    let mutable y = y
    let mutable offX = offX
    let mutable offY = offY
    let mutable n = n
    let mutable s = s
    try
        if n = 1 then
            y.[offY] <- { re = _idx x (offX); im = 0.0 }
            __ret <- ()
            raise Return
        ditfft2Rec (x) (y) (offX) (offY) (n / 2) (2 * s)
        ditfft2Rec (x) (y) (offX + s) (offY + (n / 2)) (n / 2) (2 * s)
        let mutable k: int = 0
        while k < (n / 2) do
            let angle: float = (((-2.0) * PI) * (float k)) / (float n)
            let tf: Complex = mul (cis (angle)) (_idx y ((offY + k) + (n / 2)))
            let a: Complex = add (_idx y (offY + k)) (tf)
            let b: Complex = sub (_idx y (offY + k)) (tf)
            y.[offY + k] <- a
            y.[(offY + k) + (n / 2)] <- b
            k <- k + 1
        __ret
    with
        | Return -> __ret
and ditfft2 (x: float array) (y: Complex array) (n: int) (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    let mutable y = y
    let mutable n = n
    let mutable s = s
    try
        ditfft2Rec (x) (y) (0) (0) (n) (s)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let x: float array = [|1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0|]
        let mutable y: Complex array = [||]
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            y <- Array.append y [|{ re = 0.0; im = 0.0 }|]
            i <- i + 1
        ditfft2 (x) (y) (Seq.length (x)) (1)
        for c in y do
            let mutable line: string = pad (fmt (c.re)) (8)
            if (c.im) >= (float 0) then
                line <- (line + "+") + (fmt (c.im))
            else
                line <- line + (fmt (c.im))
            printfn "%s" (line)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable t: string = s
        while (String.length (t)) < w do
            t <- " " + t
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and fmt (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (floorf ((x * 10000.0) + 0.5)) / 10000.0
        let mutable s: string = string (y)
        let mutable dot: int = s.IndexOf(".")
        if dot = (0 - 1) then
            s <- s + ".0000"
        else
            let mutable d: int = ((String.length (s)) - dot) - 1
            while d < 4 do
                s <- s + "0"
                d <- d + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and floorf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: int = int x
        __ret <- float y
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
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
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
main()
