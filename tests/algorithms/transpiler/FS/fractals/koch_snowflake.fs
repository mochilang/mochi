// Generated 2025-08-13 16:13 +0700

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
type Vec = {
    mutable _x: float
    mutable _y: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
let rec _mod (_x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    let mutable m = m
    try
        __ret <- _x - ((float (int (_x / m))) * m)
        raise Return
        __ret
    with
        | Return -> __ret
and sin (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let _y: float = (_mod (_x + PI) (TWO_PI)) - PI
        let y2: float = _y * _y
        let y3: float = y2 * _y
        let y5: float = y3 * y2
        let y7: float = y5 * y2
        __ret <- ((_y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
        raise Return
        __ret
    with
        | Return -> __ret
and cos (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let _y: float = (_mod (_x + PI) (TWO_PI)) - PI
        let y2: float = _y * _y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
and rotate (v: Vec) (angle_deg: float) =
    let mutable __ret : Vec = Unchecked.defaultof<Vec>
    let mutable v = v
    let mutable angle_deg = angle_deg
    try
        let theta: float = (angle_deg * PI) / 180.0
        let c: float = cos (theta)
        let s: float = sin (theta)
        __ret <- { _x = ((v._x) * c) - ((v._y) * s); _y = ((v._x) * s) + ((v._y) * c) }
        raise Return
        __ret
    with
        | Return -> __ret
and iteration_step (vectors: Vec array) =
    let mutable __ret : Vec array = Unchecked.defaultof<Vec array>
    let mutable vectors = vectors
    try
        let mutable new_vectors: Vec array = Array.empty<Vec>
        let mutable i: int = 0
        while i < ((Seq.length (vectors)) - 1) do
            let start: Vec = _idx vectors (int i)
            let ``end``: Vec = _idx vectors (int (i + 1))
            new_vectors <- Array.append new_vectors [|start|]
            let dx: float = (``end``._x) - (start._x)
            let dy: float = (``end``._y) - (start._y)
            let one_third: Vec = { _x = (start._x) + (dx / 3.0); _y = (start._y) + (dy / 3.0) }
            let mid: Vec = rotate ({ _x = dx / 3.0; _y = dy / 3.0 }) (60.0)
            let peak: Vec = { _x = (one_third._x) + (mid._x); _y = (one_third._y) + (mid._y) }
            let two_third: Vec = { _x = (start._x) + ((dx * 2.0) / 3.0); _y = (start._y) + ((dy * 2.0) / 3.0) }
            new_vectors <- Array.append new_vectors [|one_third|]
            new_vectors <- Array.append new_vectors [|peak|]
            new_vectors <- Array.append new_vectors [|two_third|]
            i <- i + 1
        new_vectors <- Array.append new_vectors [|(_idx vectors (int ((Seq.length (vectors)) - 1)))|]
        __ret <- new_vectors
        raise Return
        __ret
    with
        | Return -> __ret
and iterate (initial: Vec array) (steps: int) =
    let mutable __ret : Vec array = Unchecked.defaultof<Vec array>
    let mutable initial = initial
    let mutable steps = steps
    try
        let mutable vectors: Vec array = initial
        let mutable i: int = 0
        while i < steps do
            vectors <- iteration_step (vectors)
            i <- i + 1
        __ret <- vectors
        raise Return
        __ret
    with
        | Return -> __ret
and vec_to_string (v: Vec) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable v = v
    try
        __ret <- ((("(" + (_str (v._x))) + ", ") + (_str (v._y))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and vec_list_to_string (lst: Vec array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable res: string = "["
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            res <- res + (vec_to_string (_idx lst (int i)))
            if i < ((Seq.length (lst)) - 1) then
                res <- res + ", "
            i <- i + 1
        res <- res + "]"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let VECTOR_1: Vec = { _x = 0.0; _y = 0.0 }
let VECTOR_2: Vec = { _x = 0.5; _y = 0.8660254 }
let VECTOR_3: Vec = { _x = 1.0; _y = 0.0 }
let INITIAL_VECTORS: Vec array = unbox<Vec array> [|VECTOR_1; VECTOR_2; VECTOR_3; VECTOR_1|]
let example: Vec array = iterate (unbox<Vec array> [|VECTOR_1; VECTOR_3|]) (1)
ignore (printfn "%s" (vec_list_to_string (example)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
