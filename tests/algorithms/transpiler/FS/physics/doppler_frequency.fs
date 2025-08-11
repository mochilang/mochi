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
let rec doppler_effect (org_freq: float) (wave_vel: float) (obs_vel: float) (src_vel: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable org_freq = org_freq
    let mutable wave_vel = wave_vel
    let mutable obs_vel = obs_vel
    let mutable src_vel = src_vel
    try
        if wave_vel = src_vel then
            failwith ("division by zero implies vs=v and observer in front of the source")
        let doppler_freq: float = (org_freq * (wave_vel + obs_vel)) / (wave_vel - src_vel)
        if doppler_freq <= 0.0 then
            failwith ("non-positive frequency implies vs>v or v0>v (in the opposite direction)")
        __ret <- doppler_freq
        raise Return
        __ret
    with
        | Return -> __ret
and absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and almost_equal (a: float) (b: float) (tol: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable tol = tol
    try
        __ret <- (absf (a - b)) <= tol
        raise Return
        __ret
    with
        | Return -> __ret
and test_doppler_effect () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (almost_equal (doppler_effect (100.0) (330.0) (10.0) (0.0)) (103.03030303030303) (0.0000001)) then
            failwith ("test 1 failed")
        if not (almost_equal (doppler_effect (100.0) (330.0) (-10.0) (0.0)) (96.96969696969697) (0.0000001)) then
            failwith ("test 2 failed")
        if not (almost_equal (doppler_effect (100.0) (330.0) (0.0) (10.0)) (103.125) (0.0000001)) then
            failwith ("test 3 failed")
        if not (almost_equal (doppler_effect (100.0) (330.0) (0.0) (-10.0)) (97.05882352941177) (0.0000001)) then
            failwith ("test 4 failed")
        if not (almost_equal (doppler_effect (100.0) (330.0) (10.0) (10.0)) (106.25) (0.0000001)) then
            failwith ("test 5 failed")
        if not (almost_equal (doppler_effect (100.0) (330.0) (-10.0) (-10.0)) (94.11764705882354) (0.0000001)) then
            failwith ("test 6 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_doppler_effect()
        printfn "%g" (doppler_effect (100.0) (330.0) (10.0) (0.0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
