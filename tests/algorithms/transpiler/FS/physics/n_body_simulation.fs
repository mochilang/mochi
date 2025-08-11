// Generated 2025-08-09 23:14 +0700

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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
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
let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()
type Body = {
    mutable _position_x: float
    mutable _position_y: float
    mutable _velocity_x: float
    mutable _velocity_y: float
    mutable _mass: float
}
type BodySystem = {
    mutable _bodies: Body array
    mutable _gravitation_constant: float
    mutable _time_factor: float
    mutable _softening_factor: float
}
let rec make_body (px: float) (py: float) (vx: float) (vy: float) (_mass: float) =
    let mutable __ret : Body = Unchecked.defaultof<Body>
    let mutable px = px
    let mutable py = py
    let mutable vx = vx
    let mutable vy = vy
    let mutable _mass = _mass
    try
        __ret <- { _position_x = px; _position_y = py; _velocity_x = vx; _velocity_y = vy; _mass = _mass }
        raise Return
        __ret
    with
        | Return -> __ret
and update_velocity (body: Body) (force_x: float) (force_y: float) (delta_time: float) =
    let mutable __ret : Body = Unchecked.defaultof<Body>
    let mutable body = body
    let mutable force_x = force_x
    let mutable force_y = force_y
    let mutable delta_time = delta_time
    try
        body._velocity_x <- (body._velocity_x) + (force_x * delta_time)
        body._velocity_y <- (body._velocity_y) + (force_y * delta_time)
        __ret <- body
        raise Return
        __ret
    with
        | Return -> __ret
and update_position (body: Body) (delta_time: float) =
    let mutable __ret : Body = Unchecked.defaultof<Body>
    let mutable body = body
    let mutable delta_time = delta_time
    try
        body._position_x <- (body._position_x) + ((body._velocity_x) * delta_time)
        body._position_y <- (body._position_y) + ((body._velocity_y) * delta_time)
        __ret <- body
        raise Return
        __ret
    with
        | Return -> __ret
and make_body_system (_bodies: Body array) (g: float) (tf: float) (sf: float) =
    let mutable __ret : BodySystem = Unchecked.defaultof<BodySystem>
    let mutable _bodies = _bodies
    let mutable g = g
    let mutable tf = tf
    let mutable sf = sf
    try
        __ret <- { _bodies = _bodies; _gravitation_constant = g; _time_factor = tf; _softening_factor = sf }
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and update_system (system: BodySystem) (delta_time: float) =
    let mutable __ret : BodySystem = Unchecked.defaultof<BodySystem>
    let mutable system = system
    let mutable delta_time = delta_time
    try
        let mutable _bodies: Body array = system._bodies
        let mutable i: int = 0
        while i < (Seq.length (_bodies)) do
            let mutable body1: Body = _idx _bodies (int i)
            let mutable force_x: float = 0.0
            let mutable force_y: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_bodies)) do
                if i <> j then
                    let body2: Body = _idx _bodies (int j)
                    let dif_x: float = (body2._position_x) - (body1._position_x)
                    let dif_y: float = (body2._position_y) - (body1._position_y)
                    let distance_sq: float = ((dif_x * dif_x) + (dif_y * dif_y)) + (system._softening_factor)
                    let distance: float = sqrtApprox (distance_sq)
                    let denom: float = (distance * distance) * distance
                    force_x <- force_x + ((((system._gravitation_constant) * (body2._mass)) * dif_x) / denom)
                    force_y <- force_y + ((((system._gravitation_constant) * (body2._mass)) * dif_y) / denom)
                j <- j + 1
            body1 <- update_velocity (body1) (force_x) (force_y) (delta_time * (system._time_factor))
            _bodies.[int i] <- body1
            i <- i + 1
        i <- 0
        while i < (Seq.length (_bodies)) do
            let mutable body: Body = _idx _bodies (int i)
            body <- update_position (body) (delta_time * (system._time_factor))
            _bodies.[int i] <- body
            i <- i + 1
        system._bodies <- _bodies
        __ret <- system
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let b1: Body = make_body (0.0) (0.0) (0.0) (0.0) (1.0)
        let b2: Body = make_body (10.0) (0.0) (0.0) (0.0) (1.0)
        let mutable sys1: BodySystem = make_body_system (unbox<Body array> [|b1; b2|]) (1.0) (1.0) (0.0)
        sys1 <- update_system (sys1) (1.0)
        let b1_after: Body = _idx (sys1._bodies) (int 0)
        let pos1x: float = b1_after._position_x
        let pos1y: float = b1_after._position_y
        json (_dictCreate [("x", pos1x); ("y", pos1y)])
        let vel1x: float = b1_after._velocity_x
        let vel1y: float = b1_after._velocity_y
        json (_dictCreate [("vx", vel1x); ("vy", vel1y)])
        let b3: Body = make_body (-10.0) (0.0) (0.0) (0.0) (1.0)
        let b4: Body = make_body (10.0) (0.0) (0.0) (0.0) (4.0)
        let mutable sys2: BodySystem = make_body_system (unbox<Body array> [|b3; b4|]) (1.0) (10.0) (0.0)
        sys2 <- update_system (sys2) (1.0)
        let b2_after: Body = _idx (sys2._bodies) (int 0)
        let pos2x: float = b2_after._position_x
        let pos2y: float = b2_after._position_y
        json (_dictCreate [("x", pos2x); ("y", pos2y)])
        let vel2x: float = b2_after._velocity_x
        let vel2y: float = b2_after._velocity_y
        json (_dictCreate [("vx", vel2x); ("vy", vel2y)])
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
