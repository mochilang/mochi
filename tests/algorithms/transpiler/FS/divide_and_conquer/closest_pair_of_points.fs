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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (0.0 - x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec euclidean_distance_sqr (p1: float array) (p2: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        let dx: float = (_idx p1 (0)) - (_idx p2 (0))
        let dy: float = (_idx p1 (1)) - (_idx p2 (1))
        __ret <- (dx * dx) + (dy * dy)
        raise Return
        __ret
    with
        | Return -> __ret
let rec column_based_sort (arr: float array array) (column: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable arr = arr
    let mutable column = column
    try
        let mutable points: float array array = arr
        let mutable i: int = 0
        while i < (Seq.length (points)) do
            let mutable j: int = 0
            while j < ((Seq.length (points)) - 1) do
                if (_idx (_idx points (j)) (column)) > (_idx (_idx points (j + 1)) (column)) then
                    let tmp: float array = _idx points (j)
                    points <- _arrset points (j) (_idx points (j + 1))
                    points <- _arrset points (j + 1) (tmp)
                j <- j + 1
            i <- i + 1
        __ret <- points
        raise Return
        __ret
    with
        | Return -> __ret
let rec dis_between_closest_pair (points: float array array) (count: int) (min_dis: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable points = points
    let mutable count = count
    let mutable min_dis = min_dis
    try
        let mutable i: int = 0
        while i < (count - 1) do
            let mutable j: int = i + 1
            while j < count do
                let current: float = euclidean_distance_sqr (_idx points (i)) (_idx points (j))
                if current < min_dis then
                    min_dis <- current
                j <- j + 1
            i <- i + 1
        __ret <- min_dis
        raise Return
        __ret
    with
        | Return -> __ret
let rec dis_between_closest_in_strip (points: float array array) (count: int) (min_dis: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable points = points
    let mutable count = count
    let mutable min_dis = min_dis
    try
        let mutable i_start: int = 0
        if 6 < (count - 1) then
            i_start <- 6
        else
            i_start <- count - 1
        let mutable i: int = i_start
        while i < count do
            let mutable j_start: int = 0
            if (i - 6) > 0 then
                j_start <- i - 6
            let mutable j: int = j_start
            while j < i do
                let current: float = euclidean_distance_sqr (_idx points (i)) (_idx points (j))
                if current < min_dis then
                    min_dis <- current
                j <- j + 1
            i <- i + 1
        __ret <- min_dis
        raise Return
        __ret
    with
        | Return -> __ret
let rec closest_pair_of_points_sqr (px: float array array) (py: float array array) (count: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable px = px
    let mutable py = py
    let mutable count = count
    try
        if count <= 3 then
            __ret <- dis_between_closest_pair (px) (count) (1000000000000000000.0)
            raise Return
        let mid: int = count / 2
        let left: float = closest_pair_of_points_sqr (px) (Array.sub py 0 (mid - 0)) (mid)
        let right: float = closest_pair_of_points_sqr (py) (Array.sub py mid (count - mid)) (count - mid)
        let mutable best: float = left
        if right < best then
            best <- right
        let mutable strip: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (px)) do
            if (abs ((_idx (_idx px (i)) (0)) - (_idx (_idx px (mid)) (0)))) < best then
                strip <- Array.append strip [|_idx px (i)|]
            i <- i + 1
        let strip_best: float = dis_between_closest_in_strip (strip) (Seq.length (strip)) (best)
        if strip_best < best then
            best <- strip_best
        __ret <- best
        raise Return
        __ret
    with
        | Return -> __ret
let rec closest_pair_of_points (points: float array array) (count: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable points = points
    let mutable count = count
    try
        let points_sorted_on_x: float array array = column_based_sort (points) (0)
        let points_sorted_on_y: float array array = column_based_sort (points) (1)
        let dist_sqr: float = closest_pair_of_points_sqr (points_sorted_on_x) (points_sorted_on_y) (count)
        __ret <- sqrtApprox (dist_sqr)
        raise Return
        __ret
    with
        | Return -> __ret
let points: float array array = [|[|2.0; 3.0|]; [|12.0; 30.0|]; [|40.0; 50.0|]; [|5.0; 1.0|]; [|12.0; 10.0|]; [|3.0; 4.0|]|]
printfn "%s" ("Distance: " + (_str (closest_pair_of_points (points) (Seq.length (points)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
