// Generated 2025-07-26 05:05 +0700

exception Return

let rec d2d (d: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable d = d
    try
        __ret <- ((d % 360.0 + 360.0) % 360.0)
        raise Return
        __ret
    with
        | Return -> __ret
and g2g (g: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable g = g
    try
        __ret <- ((g % 400.0 + 400.0) % 400.0)
        raise Return
        __ret
    with
        | Return -> __ret
and m2m (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        __ret <- ((m % 6400.0 + 6400.0) % 6400.0)
        raise Return
        __ret
    with
        | Return -> __ret
and r2r (r: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- ((r % (2.0 * 3.141592653589793) + (2.0 * 3.141592653589793)) % (2.0 * 3.141592653589793))
        raise Return
        __ret
    with
        | Return -> __ret
and d2g (d: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable d = d
    try
        __ret <- (float ((float (d2d d)) * 400.0)) / 360.0
        raise Return
        __ret
    with
        | Return -> __ret
and d2m (d: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable d = d
    try
        __ret <- (float ((float (d2d d)) * 6400.0)) / 360.0
        raise Return
        __ret
    with
        | Return -> __ret
and d2r (d: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable d = d
    try
        __ret <- (float ((float (d2d d)) * 3.141592653589793)) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
and g2d (g: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable g = g
    try
        __ret <- (float ((float (g2g g)) * 360.0)) / 400.0
        raise Return
        __ret
    with
        | Return -> __ret
and g2m (g: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable g = g
    try
        __ret <- (float ((float (g2g g)) * 6400.0)) / 400.0
        raise Return
        __ret
    with
        | Return -> __ret
and g2r (g: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable g = g
    try
        __ret <- (float ((float (g2g g)) * 3.141592653589793)) / 200.0
        raise Return
        __ret
    with
        | Return -> __ret
and m2d (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        __ret <- (float ((float (m2m m)) * 360.0)) / 6400.0
        raise Return
        __ret
    with
        | Return -> __ret
and m2g (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        __ret <- (float ((float (m2m m)) * 400.0)) / 6400.0
        raise Return
        __ret
    with
        | Return -> __ret
and m2r (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        __ret <- (float ((float (m2m m)) * 3.141592653589793)) / 3200.0
        raise Return
        __ret
    with
        | Return -> __ret
and r2d (r: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- (float ((float (r2r r)) * 180.0)) / 3.141592653589793
        raise Return
        __ret
    with
        | Return -> __ret
and r2g (r: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- (float ((float (r2r r)) * 200.0)) / 3.141592653589793
        raise Return
        __ret
    with
        | Return -> __ret
and r2m (r: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- (float ((float (r2r r)) * 3200.0)) / 3.141592653589793
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let angles: float array = [|-2.0; -1.0; 0.0; 1.0; 2.0; 6.2831853; 16.0; 57.2957795; 359.0; 399.0; 6399.0; 1000000.0|]
        printfn "%s" "degrees normalized_degs gradians mils radians"
        for a in angles do
            printfn "%s" (((((((((string a) + " ") + (string (d2d (float a)))) + " ") + (string (d2g (float a)))) + " ") + (string (d2m (float a)))) + " ") + (string (d2r (float a))))
        printfn "%s" "\ngradians normalized_grds degrees mils radians"
        for a in angles do
            printfn "%s" (((((((((string a) + " ") + (string (g2g a))) + " ") + (string (g2d a))) + " ") + (string (g2m a))) + " ") + (string (g2r a)))
        printfn "%s" "\nmils normalized_mils degrees gradians radians"
        for a in angles do
            printfn "%s" (((((((((string a) + " ") + (string (m2m a))) + " ") + (string (m2d a))) + " ") + (string (m2g a))) + " ") + (string (m2r a)))
        printfn "%s" "\nradians normalized_rads degrees gradians mils"
        for a in angles do
            printfn "%s" (((((((((string a) + " ") + (string (r2r a))) + " ") + (string (r2d a))) + " ") + (string (r2g a))) + " ") + (string (r2m a)))
        __ret
    with
        | Return -> __ret
main()
