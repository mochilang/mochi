// Generated 2025-08-08 18:09 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
let rec validate_inputs (x_initials: float array) (step_size: float) (x_final: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x_initials = x_initials
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        if (_idx x_initials ((Seq.length (x_initials)) - 1)) >= x_final then
            failwith ("The final value of x must be greater than the initial values of x.")
        if step_size <= 0.0 then
            failwith ("Step size must be positive.")
        let mutable i: int = 0
        while i < ((Seq.length (x_initials)) - 1) do
            let diff: float = (_idx x_initials (i + 1)) - (_idx x_initials (i))
            if (abs_float (diff - step_size)) > 0.0000000001 then
                failwith ("x-values must be equally spaced according to step size.")
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec list_to_string (xs: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_str (_idx xs (i)))
            if (i + 1) < (Seq.length (xs)) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec adams_bashforth_step2 (f: float -> float -> float) (x_initials: float array) (y_initials: float array) (step_size: float) (x_final: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable f = f
    let mutable x_initials = x_initials
    let mutable y_initials = y_initials
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        validate_inputs (x_initials) (step_size) (x_final)
        if ((Seq.length (x_initials)) <> 2) || ((Seq.length (y_initials)) <> 2) then
            failwith ("Insufficient initial points information.")
        let mutable x0: float = _idx x_initials (0)
        let mutable x1: float = _idx x_initials (1)
        let mutable y: float array = Array.empty<float>
        y <- Array.append y [|(_idx y_initials (0))|]
        y <- Array.append y [|(_idx y_initials (1))|]
        let n: int = int ((x_final - x1) / step_size)
        let mutable i: int = 0
        while i < n do
            let term: float = (float (3.0 * (float (f (x1) (_idx y (i + 1)))))) - (float (f (x0) (_idx y (i))))
            let y_next: float = (_idx y (i + 1)) + ((step_size / 2.0) * term)
            y <- Array.append y [|y_next|]
            x0 <- x1
            x1 <- x1 + step_size
            i <- i + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let rec adams_bashforth_step3 (f: float -> float -> float) (x_initials: float array) (y_initials: float array) (step_size: float) (x_final: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable f = f
    let mutable x_initials = x_initials
    let mutable y_initials = y_initials
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        validate_inputs (x_initials) (step_size) (x_final)
        if ((Seq.length (x_initials)) <> 3) || ((Seq.length (y_initials)) <> 3) then
            failwith ("Insufficient initial points information.")
        let mutable x0: float = _idx x_initials (0)
        let mutable x1: float = _idx x_initials (1)
        let mutable x2: float = _idx x_initials (2)
        let mutable y: float array = Array.empty<float>
        y <- Array.append y [|(_idx y_initials (0))|]
        y <- Array.append y [|(_idx y_initials (1))|]
        y <- Array.append y [|(_idx y_initials (2))|]
        let n: int = int ((x_final - x2) / step_size)
        let mutable i: int = 0
        while i <= n do
            let term: float = (float ((float (23.0 * (float (f (x2) (_idx y (i + 2)))))) - (float (16.0 * (float (f (x1) (_idx y (i + 1)))))))) + (float (5.0 * (float (f (x0) (_idx y (i))))))
            let y_next: float = (_idx y (i + 2)) + ((step_size / 12.0) * term)
            y <- Array.append y [|y_next|]
            x0 <- x1
            x1 <- x2
            x2 <- x2 + step_size
            i <- i + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let rec adams_bashforth_step4 (f: float -> float -> float) (x_initials: float array) (y_initials: float array) (step_size: float) (x_final: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable f = f
    let mutable x_initials = x_initials
    let mutable y_initials = y_initials
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        validate_inputs (x_initials) (step_size) (x_final)
        if ((Seq.length (x_initials)) <> 4) || ((Seq.length (y_initials)) <> 4) then
            failwith ("Insufficient initial points information.")
        let mutable x0: float = _idx x_initials (0)
        let mutable x1: float = _idx x_initials (1)
        let mutable x2: float = _idx x_initials (2)
        let mutable x3: float = _idx x_initials (3)
        let mutable y: float array = Array.empty<float>
        y <- Array.append y [|(_idx y_initials (0))|]
        y <- Array.append y [|(_idx y_initials (1))|]
        y <- Array.append y [|(_idx y_initials (2))|]
        y <- Array.append y [|(_idx y_initials (3))|]
        let n: int = int ((x_final - x3) / step_size)
        let mutable i: int = 0
        while i < n do
            let term: float = (float ((float ((float (55.0 * (float (f (x3) (_idx y (i + 3)))))) - (float (59.0 * (float (f (x2) (_idx y (i + 2)))))))) + (float (37.0 * (float (f (x1) (_idx y (i + 1)))))))) - (float (9.0 * (float (f (x0) (_idx y (i))))))
            let y_next: float = (_idx y (i + 3)) + ((step_size / 24.0) * term)
            y <- Array.append y [|y_next|]
            x0 <- x1
            x1 <- x2
            x2 <- x3
            x3 <- x3 + step_size
            i <- i + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let rec adams_bashforth_step5 (f: float -> float -> float) (x_initials: float array) (y_initials: float array) (step_size: float) (x_final: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable f = f
    let mutable x_initials = x_initials
    let mutable y_initials = y_initials
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        validate_inputs (x_initials) (step_size) (x_final)
        if ((Seq.length (x_initials)) <> 5) || ((Seq.length (y_initials)) <> 5) then
            failwith ("Insufficient initial points information.")
        let mutable x0: float = _idx x_initials (0)
        let mutable x1: float = _idx x_initials (1)
        let mutable x2: float = _idx x_initials (2)
        let mutable x3: float = _idx x_initials (3)
        let mutable x4: float = _idx x_initials (4)
        let mutable y: float array = Array.empty<float>
        y <- Array.append y [|(_idx y_initials (0))|]
        y <- Array.append y [|(_idx y_initials (1))|]
        y <- Array.append y [|(_idx y_initials (2))|]
        y <- Array.append y [|(_idx y_initials (3))|]
        y <- Array.append y [|(_idx y_initials (4))|]
        let n: int = int ((x_final - x4) / step_size)
        let mutable i: int = 0
        while i <= n do
            let term: float = (float ((float ((float ((float (1901.0 * (float (f (x4) (_idx y (i + 4)))))) - (float (2774.0 * (float (f (x3) (_idx y (i + 3)))))))) - (float (2616.0 * (float (f (x2) (_idx y (i + 2)))))))) - (float (1274.0 * (float (f (x1) (_idx y (i + 1)))))))) + (float (251.0 * (float (f (x0) (_idx y (i))))))
            let y_next: float = (_idx y (i + 4)) + ((step_size / 720.0) * term)
            y <- Array.append y [|y_next|]
            x0 <- x1
            x1 <- x2
            x2 <- x3
            x3 <- x4
            x4 <- x4 + step_size
            i <- i + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let rec f_x (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec f_xy (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- x + y
        raise Return
        __ret
    with
        | Return -> __ret
let y2: float array = adams_bashforth_step2 (unbox<float -> float -> float> f_x) (unbox<float array> [|0.0; 0.2|]) (unbox<float array> [|0.0; 0.0|]) (0.2) (1.0)
printfn "%s" (list_to_string (y2))
let y3: float array = adams_bashforth_step3 (unbox<float -> float -> float> f_xy) (unbox<float array> [|0.0; 0.2; 0.4|]) (unbox<float array> [|0.0; 0.0; 0.04|]) (0.2) (1.0)
printfn "%s" (_str (_idx y3 (3)))
let y4: float array = adams_bashforth_step4 (unbox<float -> float -> float> f_xy) (unbox<float array> [|0.0; 0.2; 0.4; 0.6|]) (unbox<float array> [|0.0; 0.0; 0.04; 0.128|]) (0.2) (1.0)
printfn "%s" (_str (_idx y4 (4)))
printfn "%s" (_str (_idx y4 (5)))
let y5: float array = adams_bashforth_step5 (unbox<float -> float -> float> f_xy) (unbox<float array> [|0.0; 0.2; 0.4; 0.6; 0.8|]) (unbox<float array> [|0.0; 0.0214; 0.0214; 0.22211; 0.42536|]) (0.2) (1.0)
printfn "%s" (_str (_idx y5 ((Seq.length (y5)) - 1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
