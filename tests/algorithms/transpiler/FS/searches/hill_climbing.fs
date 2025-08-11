// Generated 2025-08-11 16:20 +0700

exception Break
exception Continue

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
type SearchProblem = {
    mutable _x: float
    mutable _y: float
    mutable _step: float
    mutable _f: float -> float -> float
}
let rec score (sp: SearchProblem) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable sp = sp
    try
        __ret <- float (sp._f (sp._x) (sp._y))
        raise Return
        __ret
    with
        | Return -> __ret
and neighbors (sp: SearchProblem) =
    let mutable __ret : SearchProblem array = Unchecked.defaultof<SearchProblem array>
    let mutable sp = sp
    try
        let s: float = sp._step
        __ret <- unbox<SearchProblem array> [|{ _x = (sp._x) - s; _y = (sp._y) - s; _step = s; _f = sp._f }; { _x = (sp._x) - s; _y = sp._y; _step = s; _f = sp._f }; { _x = (sp._x) - s; _y = (sp._y) + s; _step = s; _f = sp._f }; { _x = sp._x; _y = (sp._y) - s; _step = s; _f = sp._f }; { _x = sp._x; _y = (sp._y) + s; _step = s; _f = sp._f }; { _x = (sp._x) + s; _y = (sp._y) - s; _step = s; _f = sp._f }; { _x = (sp._x) + s; _y = sp._y; _step = s; _f = sp._f }; { _x = (sp._x) + s; _y = (sp._y) + s; _step = s; _f = sp._f }|]
        raise Return
        __ret
    with
        | Return -> __ret
and equal_state (a: SearchProblem) (b: SearchProblem) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- ((a._x) = (b._x)) && ((a._y) = (b._y))
        raise Return
        __ret
    with
        | Return -> __ret
and contains_state (lst: SearchProblem array) (sp: SearchProblem) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable lst = lst
    let mutable sp = sp
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if equal_state (_idx lst (int i)) (sp) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and hill_climbing (sp: SearchProblem) (find_max: bool) (max_x: float) (min_x: float) (max_y: float) (min_y: float) (max_iter: int) =
    let mutable __ret : SearchProblem = Unchecked.defaultof<SearchProblem>
    let mutable sp = sp
    let mutable find_max = find_max
    let mutable max_x = max_x
    let mutable min_x = min_x
    let mutable max_y = max_y
    let mutable min_y = min_y
    let mutable max_iter = max_iter
    try
        let mutable current: SearchProblem = sp
        let mutable visited: SearchProblem array = Array.empty<SearchProblem>
        let mutable iterations: int = 0
        let mutable solution_found: bool = false
        try
            while (solution_found = false) && (iterations < max_iter) do
                try
                    visited <- Array.append visited [|current|]
                    iterations <- iterations + 1
                    let current_score: float = score (current)
                    let neighs: SearchProblem array = neighbors (current)
                    let mutable max_change: float = -1000000000000000000.0
                    let mutable min_change: float = 1000000000000000000.0
                    let mutable next: SearchProblem = current
                    let mutable improved: bool = false
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (neighs)) do
                            try
                                let n: SearchProblem = _idx neighs (int i)
                                i <- i + 1
                                if contains_state (visited) (n) then
                                    raise Continue
                                if ((((n._x) > max_x) || ((n._x) < min_x)) || ((n._y) > max_y)) || ((n._y) < min_y) then
                                    raise Continue
                                let change: float = (score (n)) - current_score
                                if find_max then
                                    if (change > max_change) && (change > 0.0) then
                                        max_change <- change
                                        next <- n
                                        improved <- true
                                else
                                    if (change < min_change) && (change < 0.0) then
                                        min_change <- change
                                        next <- n
                                        improved <- true
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if improved then
                        current <- next
                    else
                        solution_found <- true
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- current
        raise Return
        __ret
    with
        | Return -> __ret
and test_f1 (_x: float) (_y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    let mutable _y = _y
    try
        __ret <- (_x * _x) + (_y * _y)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let prob1: SearchProblem = { _x = 3.0; _y = 4.0; _step = 1.0; _f = unbox<float -> float -> float> test_f1 }
        let local_min1: SearchProblem = hill_climbing (prob1) (false) (1000000000.0) (-1000000000.0) (1000000000.0) (-1000000000.0) (10000)
        printfn "%s" (_str (int (score (local_min1))))
        let prob2: SearchProblem = { _x = 12.0; _y = 47.0; _step = 1.0; _f = unbox<float -> float -> float> test_f1 }
        let local_min2: SearchProblem = hill_climbing (prob2) (false) (100.0) (5.0) (50.0) (-5.0) (10000)
        printfn "%s" (_str (int (score (local_min2))))
        let prob3: SearchProblem = { _x = 3.0; _y = 4.0; _step = 1.0; _f = unbox<float -> float -> float> test_f1 }
        let local_max: SearchProblem = hill_climbing (prob3) (true) (1000000000.0) (-1000000000.0) (1000000000.0) (-1000000000.0) (1000)
        printfn "%s" (_str (int (score (local_max))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
