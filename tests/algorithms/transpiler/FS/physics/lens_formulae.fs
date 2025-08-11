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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec focal_length_of_lens (object_distance_from_lens: float) (image_distance_from_lens: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable object_distance_from_lens = object_distance_from_lens
    let mutable image_distance_from_lens = image_distance_from_lens
    try
        if (object_distance_from_lens = 0.0) || (image_distance_from_lens = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / object_distance_from_lens))
        raise Return
        __ret
    with
        | Return -> __ret
let rec object_distance (focal_length_of_lens: float) (image_distance_from_lens: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable focal_length_of_lens = focal_length_of_lens
    let mutable image_distance_from_lens = image_distance_from_lens
    try
        if (image_distance_from_lens = 0.0) || (focal_length_of_lens = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / focal_length_of_lens))
        raise Return
        __ret
    with
        | Return -> __ret
let rec image_distance (focal_length_of_lens: float) (object_distance_from_lens: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable focal_length_of_lens = focal_length_of_lens
    let mutable object_distance_from_lens = object_distance_from_lens
    try
        if (object_distance_from_lens = 0.0) || (focal_length_of_lens = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / object_distance_from_lens) + (1.0 / focal_length_of_lens))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (focal_length_of_lens (10.0) (4.0)))
printfn "%s" (_str (focal_length_of_lens (2.7) (5.8)))
printfn "%s" (_str (object_distance (10.0) (40.0)))
printfn "%s" (_str (object_distance (6.2) (1.5)))
printfn "%s" (_str (image_distance (50.0) (40.0)))
printfn "%s" (_str (image_distance (5.3) (7.9)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
