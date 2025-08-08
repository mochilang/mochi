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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Polynomial = {
    mutable _degree: int
    mutable _coefficients: float array
}
let rec copy_list (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and polynomial_new (_degree: int) (coeffs: float array) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable _degree = _degree
    let mutable coeffs = coeffs
    try
        if (Seq.length (coeffs)) <> (_degree + 1) then
            failwith ("The number of coefficients should be equal to the degree + 1.")
        __ret <- { _degree = _degree; _coefficients = copy_list (coeffs) }
        raise Return
        __ret
    with
        | Return -> __ret
and add (p: Polynomial) (q: Polynomial) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    let mutable q = q
    try
        if (p._degree) > (q._degree) then
            let mutable coeffs: float array = copy_list (p._coefficients)
            let mutable i: int = 0
            while i <= (q._degree) do
                coeffs.[int i] <- (_idx coeffs (int i)) + (_idx (q._coefficients) (int i))
                i <- i + 1
            __ret <- { _degree = p._degree; _coefficients = coeffs }
            raise Return
        else
            let mutable coeffs: float array = copy_list (q._coefficients)
            let mutable i: int = 0
            while i <= (p._degree) do
                coeffs.[int i] <- (_idx coeffs (int i)) + (_idx (p._coefficients) (int i))
                i <- i + 1
            __ret <- { _degree = q._degree; _coefficients = coeffs }
            raise Return
        __ret
    with
        | Return -> __ret
and neg (p: Polynomial) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    try
        let mutable coeffs: float array = Array.empty<float>
        let mutable i: int = 0
        while i <= (p._degree) do
            coeffs <- Array.append coeffs [|(-(_idx (p._coefficients) (int i)))|]
            i <- i + 1
        __ret <- { _degree = p._degree; _coefficients = coeffs }
        raise Return
        __ret
    with
        | Return -> __ret
and sub (p: Polynomial) (q: Polynomial) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    let mutable q = q
    try
        __ret <- add (p) (neg (q))
        raise Return
        __ret
    with
        | Return -> __ret
and mul (p: Polynomial) (q: Polynomial) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    let mutable q = q
    try
        let mutable size: int = ((p._degree) + (q._degree)) + 1
        let mutable coeffs: float array = Array.empty<float>
        let mutable i: int = 0
        while i < size do
            coeffs <- Array.append coeffs [|0.0|]
            i <- i + 1
        i <- 0
        while i <= (p._degree) do
            let mutable j: int = 0
            while j <= (q._degree) do
                coeffs.[int (i + j)] <- (_idx coeffs (int (i + j))) + ((_idx (p._coefficients) (int i)) * (_idx (q._coefficients) (int j)))
                j <- j + 1
            i <- i + 1
        __ret <- { _degree = (p._degree) + (q._degree); _coefficients = coeffs }
        raise Return
        __ret
    with
        | Return -> __ret
and power (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and evaluate (p: Polynomial) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable x = x
    try
        let mutable result: float = 0.0
        let mutable i: int = 0
        while i <= (p._degree) do
            result <- result + ((_idx (p._coefficients) (int i)) * (power (x) (i)))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and poly_to_string (p: Polynomial) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        let mutable s: string = ""
        let mutable i: int = p._degree
        while i >= 0 do
            let coeff: float = _idx (p._coefficients) (int i)
            if coeff <> 0.0 then
                if (String.length (s)) > 0 then
                    if coeff > 0.0 then
                        s <- s + " + "
                    else
                        s <- s + " - "
                else
                    if coeff < 0.0 then
                        s <- s + "-"
                let abs_coeff: float = if coeff < 0.0 then (-coeff) else coeff
                if i = 0 then
                    s <- s + (_str (abs_coeff))
                else
                    if i = 1 then
                        s <- (s + (_str (abs_coeff))) + "x"
                    else
                        s <- ((s + (_str (abs_coeff))) + "x^") + (_str (i))
            i <- i - 1
        if s = "" then
            s <- "0"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and derivative (p: Polynomial) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    try
        if (p._degree) = 0 then
            __ret <- { _degree = 0; _coefficients = unbox<float array> [|0.0|] }
            raise Return
        let mutable coeffs: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (p._degree) do
            coeffs <- Array.append coeffs [|unbox<float> ((_idx (p._coefficients) (int (i + 1))) * (float (float (i + 1))))|]
            i <- i + 1
        __ret <- { _degree = (p._degree) - 1; _coefficients = coeffs }
        raise Return
        __ret
    with
        | Return -> __ret
and integral (p: Polynomial) (constant: float) =
    let mutable __ret : Polynomial = Unchecked.defaultof<Polynomial>
    let mutable p = p
    let mutable constant = constant
    try
        let mutable coeffs: float array = unbox<float array> [|constant|]
        let mutable i: int = 0
        while i <= (p._degree) do
            coeffs <- Array.append coeffs [|unbox<float> ((_idx (p._coefficients) (int i)) / (float (float (i + 1))))|]
            i <- i + 1
        __ret <- { _degree = (p._degree) + 1; _coefficients = coeffs }
        raise Return
        __ret
    with
        | Return -> __ret
and equals (p: Polynomial) (q: Polynomial) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    let mutable q = q
    try
        if (p._degree) <> (q._degree) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i <= (p._degree) do
            if (_idx (p._coefficients) (int i)) <> (_idx (q._coefficients) (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and not_equals (p: Polynomial) (q: Polynomial) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    let mutable q = q
    try
        __ret <- not (equals (p) (q))
        raise Return
        __ret
    with
        | Return -> __ret
and test_polynomial () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let p: Polynomial = polynomial_new (2) (unbox<float array> [|1.0; 2.0; 3.0|])
        let q: Polynomial = polynomial_new (2) (unbox<float array> [|1.0; 2.0; 3.0|])
        if (poly_to_string (add (p) (q))) <> "6x^2 + 4x + 2" then
            failwith ("add failed")
        if (poly_to_string (sub (p) (q))) <> "0" then
            failwith ("sub failed")
        if (evaluate (p) (2.0)) <> 17.0 then
            failwith ("evaluate failed")
        if (poly_to_string (derivative (p))) <> "6x + 2" then
            failwith ("derivative failed")
        let integ: string = poly_to_string (integral (p) (0.0))
        if integ <> "1x^3 + 1x^2 + 1x" then
            failwith ("integral failed")
        if not (equals (p) (q)) then
            failwith ("equals failed")
        if not_equals (p) (q) then
            failwith ("not_equals failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_polynomial()
        let p: Polynomial = polynomial_new (2) (unbox<float array> [|1.0; 2.0; 3.0|])
        let d: Polynomial = derivative (p)
        printfn "%s" (poly_to_string (d))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
