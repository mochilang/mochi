// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec mat_inverse3 (m: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let a: float = _idx (_idx m (0)) (0)
        let b: float = _idx (_idx m (0)) (1)
        let mutable c: float = _idx (_idx m (0)) (2)
        let d: float = _idx (_idx m (1)) (0)
        let e: float = _idx (_idx m (1)) (1)
        let f: float = _idx (_idx m (1)) (2)
        let g: float = _idx (_idx m (2)) (0)
        let h: float = _idx (_idx m (2)) (1)
        let mutable i: float = _idx (_idx m (2)) (2)
        let det: float = ((a * ((e * i) - (f * h))) - (b * ((d * i) - (f * g)))) + (c * ((d * h) - (e * g)))
        if det = 0.0 then
            failwith ("singular matrix")
        let adj00: float = (e * i) - (f * h)
        let adj01: float = (c * h) - (b * i)
        let adj02: float = (b * f) - (c * e)
        let adj10: float = (f * g) - (d * i)
        let adj11: float = (a * i) - (c * g)
        let adj12: float = (c * d) - (a * f)
        let adj20: float = (d * h) - (e * g)
        let adj21: float = (b * g) - (a * h)
        let adj22: float = (a * e) - (b * d)
        let mutable inv: float array array = [||]
        inv <- Array.append inv [|[|adj00 / det; adj01 / det; adj02 / det|]|]
        inv <- Array.append inv [|[|adj10 / det; adj11 / det; adj12 / det|]|]
        inv <- Array.append inv [|[|adj20 / det; adj21 / det; adj22 / det|]|]
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
let rec mat_vec_mul (m: float array array) (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable m = m
    let mutable v = v
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < 3 do
            let ``val``: float = (((_idx (_idx m (i)) (0)) * (_idx v (0))) + ((_idx (_idx m (i)) (1)) * (_idx v (1)))) + ((_idx (_idx m (i)) (2)) * (_idx v (2)))
            res <- Array.append res [|``val``|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec create_matrix (rows: int) (cols: int) (value: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable rows = rows
    let mutable cols = cols
    let mutable value = value
    try
        let mutable result: int array array = [||]
        let mutable r: int = 0
        while r < rows do
            let mutable row: int array = [||]
            let mutable c: int = 0
            while c < cols do
                row <- Array.append row [|value|]
                c <- c + 1
            result <- Array.append result [|row|]
            r <- r + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec round_to_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then (int (x + 0.5)) else (int (x - 0.5))
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_rotation (img: int array array) (pt1: float array array) (pt2: float array array) (rows: int) (cols: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable pt1 = pt1
    let mutable pt2 = pt2
    let mutable rows = rows
    let mutable cols = cols
    try
        let src: float array array = [|[|_idx (_idx pt1 (0)) (0); _idx (_idx pt1 (0)) (1); 1.0|]; [|_idx (_idx pt1 (1)) (0); _idx (_idx pt1 (1)) (1); 1.0|]; [|_idx (_idx pt1 (2)) (0); _idx (_idx pt1 (2)) (1); 1.0|]|]
        let mutable inv: float array array = mat_inverse3 (src)
        let vecx: float array = [|_idx (_idx pt2 (0)) (0); _idx (_idx pt2 (1)) (0); _idx (_idx pt2 (2)) (0)|]
        let vecy: float array = [|_idx (_idx pt2 (0)) (1); _idx (_idx pt2 (1)) (1); _idx (_idx pt2 (2)) (1)|]
        let avec: float array = mat_vec_mul (inv) (vecx)
        let bvec: float array = mat_vec_mul (inv) (vecy)
        let a0: float = _idx avec (0)
        let a1: float = _idx avec (1)
        let a2: float = _idx avec (2)
        let b0: float = _idx bvec (0)
        let b1: float = _idx bvec (1)
        let b2: float = _idx bvec (2)
        let mutable out: int array array = create_matrix (rows) (cols) (0)
        let mutable y: int = 0
        while y < rows do
            let mutable x: int = 0
            while x < cols do
                let xf: float = ((a0 * (1.0 * (float x))) + (a1 * (1.0 * (float y)))) + a2
                let yf: float = ((b0 * (1.0 * (float x))) + (b1 * (1.0 * (float y)))) + b2
                let sx: int = round_to_int (xf)
                let sy: int = round_to_int (yf)
                if (((sx >= 0) && (sx < cols)) && (sy >= 0)) && (sy < rows) then
                    out.[sy].[sx] <- _idx (_idx img (y)) (x)
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let img: int array array = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]|]
let pts1: float array array = [|[|0.0; 0.0|]; [|2.0; 0.0|]; [|0.0; 2.0|]|]
let pts2: float array array = [|[|0.0; 2.0|]; [|0.0; 0.0|]; [|2.0; 2.0|]|]
let rotated: int array array = get_rotation (img) (pts1) (pts2) (3) (3)
printfn "%s" (_str (rotated))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
