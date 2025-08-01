// Generated 2025-08-01 18:27 +0700

exception Return

let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

type Fps = {
    coeffs: float array
    compute: int -> float
}
type Pair = {
    sin: Fps
    cos: Fps
}
let rec newFps (fn: int -> float) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable fn = fn
    try
        __ret <- { coeffs = [||]; compute = fn }
        raise Return
        __ret
    with
        | Return -> __ret
and extract (f: Fps) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable n = n
    try
        while (Seq.length (f.coeffs)) <= n do
            let idx: int = Seq.length (f.coeffs)
            let v = f.compute idx
            f <- { f with coeffs = Array.append (f.coeffs) [|unbox<float> (v)|] }
        __ret <- (f.coeffs).[n]
        raise Return
        __ret
    with
        | Return -> __ret
and one () =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    try
        __ret <- newFps (unbox<int -> float> (        fun (i: int) -> 
            let mutable __ret : float = Unchecked.defaultof<float>
            try
                if i = 0 then
                    __ret <- 1.0
                    raise Return
                __ret <- 0.0
                raise Return
                __ret
            with
                | Return -> __ret))
        raise Return
        __ret
    with
        | Return -> __ret
and add (a: Fps) (b: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    let mutable b = b
    try
        __ret <- newFps (unbox<int -> float> (        fun (n: int) -> ((extract a n) + (extract b n))))
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: Fps) (b: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    let mutable b = b
    try
        __ret <- newFps (unbox<int -> float> (        fun (n: int) -> ((extract a n) - (extract b n))))
        raise Return
        __ret
    with
        | Return -> __ret
and mul (a: Fps) (b: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    let mutable b = b
    try
        __ret <- newFps (unbox<int -> float> (        fun (n: int) -> 
            let mutable __ret : float = Unchecked.defaultof<float>
            try
                let mutable s: float = 0.0
                let mutable k: int = 0
                while k <= n do
                    s <- s + (float ((extract a k) * (extract b (n - k))))
                    k <- k + 1
                __ret <- s
                raise Return
                __ret
            with
                | Return -> __ret))
        raise Return
        __ret
    with
        | Return -> __ret
and div (a: Fps) (b: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    let mutable b = b
    try
        let mutable q: Fps = newFps (unbox<int -> float> (        fun (n: int) -> 0.0))
        q <- { q with compute =         fun (n: int) -> 
            let mutable __ret : float = Unchecked.defaultof<float>
            try
                let b0: float = extract b 0
                if b0 = 0.0 then
                    __ret <- 0.0 / 0.0
                    raise Return
                let mutable s: float = extract a n
                let mutable k: int = 1
                while k <= n do
                    s <- s - (float ((extract b k) * (extract q (n - k))))
                    k <- k + 1
                __ret <- s / b0
                raise Return
                __ret
            with
                | Return -> __ret }
        __ret <- q
        raise Return
        __ret
    with
        | Return -> __ret
and differentiate (a: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    try
        __ret <- newFps (unbox<int -> float> (        fun (n: int) -> ((float (n + 1)) * (float (extract a (n + 1))))))
        raise Return
        __ret
    with
        | Return -> __ret
and integrate (a: Fps) =
    let mutable __ret : Fps = Unchecked.defaultof<Fps>
    let mutable a = a
    try
        __ret <- newFps (unbox<int -> float> (        fun (n: int) -> 
            let mutable __ret : float = Unchecked.defaultof<float>
            try
                if n = 0 then
                    __ret <- 0.0
                    raise Return
                __ret <- (float (extract a (n - 1))) / (float n)
                raise Return
                __ret
            with
                | Return -> __ret))
        raise Return
        __ret
    with
        | Return -> __ret
and sinCos () =
    let mutable __ret : Pair = Unchecked.defaultof<Pair>
    try
        let mutable sin: Fps = newFps (unbox<int -> float> (        fun (n: int) -> 0.0))
        let mutable cos: Fps = sub (one()) (integrate sin)
        sin <- { sin with compute =         fun (n: int) -> 
            let mutable __ret : float = Unchecked.defaultof<float>
            try
                if n = 0 then
                    __ret <- 0.0
                    raise Return
                __ret <- (float (extract cos (n - 1))) / (float n)
                raise Return
                __ret
            with
                | Return -> __ret }
        __ret <- { sin = sin; cos = cos }
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
and fmtF5 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (floorf ((x * 100000.0) + 0.5))) / 100000.0
        let mutable s: string = string y
        let mutable dot: int = s.IndexOf(".")
        if dot = (0 - 1) then
            s <- s + ".00000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            if decs > 5 then
                s <- _substring s 0 (dot + 6)
            else
                while decs < 5 do
                    s <- s + "0"
                    decs <- decs + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padFloat5 (x: float) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable width = width
    try
        let mutable s: string = fmtF5 x
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and partialSeries (f: Fps) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < 6 do
            out <- ((out + " ") + (unbox<string> (padFloat5 (extract f i) 8))) + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let p: Pair = sinCos()
        printfn "%s" ("sin:" + (unbox<string> (partialSeries (p.sin))))
        printfn "%s" ("cos:" + (unbox<string> (partialSeries (p.cos))))
        __ret
    with
        | Return -> __ret
main()
