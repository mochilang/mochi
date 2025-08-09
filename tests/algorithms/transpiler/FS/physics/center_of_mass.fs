// Generated 2025-08-09 16:21 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Particle = {
    mutable x: float
    mutable y: float
    mutable z: float
    mutable _mass: float
}
type Coord3D = {
    mutable x: float
    mutable y: float
    mutable z: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec round2 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let scaled: float = x * 100.0
        let rounded: float = float (int (scaled + 0.5))
        __ret <- rounded / 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and center_of_mass (ps: Particle array) =
    let mutable __ret : Coord3D = Unchecked.defaultof<Coord3D>
    let mutable ps = ps
    try
        if (Seq.length (ps)) = 0 then
            failwith ("No particles provided")
        let mutable i: int = 0
        let mutable total_mass: float = 0.0
        while i < (Seq.length (ps)) do
            let p: Particle = _idx ps (int i)
            if (p._mass) <= 0.0 then
                failwith ("Mass of all particles must be greater than 0")
            total_mass <- total_mass + (p._mass)
            i <- i + 1
        let mutable sum_x: float = 0.0
        let mutable sum_y: float = 0.0
        let mutable sum_z: float = 0.0
        i <- 0
        while i < (Seq.length (ps)) do
            let p: Particle = _idx ps (int i)
            sum_x <- sum_x + ((p.x) * (p._mass))
            sum_y <- sum_y + ((p.y) * (p._mass))
            sum_z <- sum_z + ((p.z) * (p._mass))
            i <- i + 1
        let cm_x: float = round2 (sum_x / total_mass)
        let cm_y: float = round2 (sum_y / total_mass)
        let cm_z: float = round2 (sum_z / total_mass)
        __ret <- { x = cm_x; y = cm_y; z = cm_z }
        raise Return
        __ret
    with
        | Return -> __ret
and coord_to_string (c: Coord3D) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        __ret <- ((((("Coord3D(x=" + (_str (c.x))) + ", y=") + (_str (c.y))) + ", z=") + (_str (c.z))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Coord3D = center_of_mass (unbox<Particle array> [|{ x = 1.5; y = 4.0; z = 3.4; _mass = 4.0 }; { x = 5.0; y = 6.8; z = 7.0; _mass = 8.1 }; { x = 9.4; y = 10.1; z = 11.6; _mass = 12.0 }|])
printfn "%s" (coord_to_string (r1))
let r2: Coord3D = center_of_mass (unbox<Particle array> [|{ x = 1.0; y = 2.0; z = 3.0; _mass = 4.0 }; { x = 5.0; y = 6.0; z = 7.0; _mass = 8.0 }; { x = 9.0; y = 10.0; z = 11.0; _mass = 12.0 }|])
printfn "%s" (coord_to_string (r2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
