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
type SearchProblem = {
    mutable _x: float
    mutable _y: float
    mutable _step: float
}
open System

let rec score (p: SearchProblem) (f: float -> float -> float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable f = f
    try
        __ret <- f (p._x) (p._y)
        raise Return
        __ret
    with
        | Return -> __ret
and get_neighbors (p: SearchProblem) =
    let mutable __ret : SearchProblem array = Unchecked.defaultof<SearchProblem array>
    let mutable p = p
    try
        let s: float = p._step
        let mutable ns: SearchProblem array = Array.empty<SearchProblem>
        ns <- Array.append ns [|{ _x = (p._x) - s; _y = (p._y) - s; _step = s }|]
        ns <- Array.append ns [|{ _x = (p._x) - s; _y = p._y; _step = s }|]
        ns <- Array.append ns [|{ _x = (p._x) - s; _y = (p._y) + s; _step = s }|]
        ns <- Array.append ns [|{ _x = p._x; _y = (p._y) - s; _step = s }|]
        ns <- Array.append ns [|{ _x = p._x; _y = (p._y) + s; _step = s }|]
        ns <- Array.append ns [|{ _x = (p._x) + s; _y = (p._y) - s; _step = s }|]
        ns <- Array.append ns [|{ _x = (p._x) + s; _y = p._y; _step = s }|]
        ns <- Array.append ns [|{ _x = (p._x) + s; _y = (p._y) + s; _step = s }|]
        __ret <- ns
        raise Return
        __ret
    with
        | Return -> __ret
and remove_at (lst: SearchProblem array) (idx: int) =
    let mutable __ret : SearchProblem array = Unchecked.defaultof<SearchProblem array>
    let mutable lst = lst
    let mutable idx = idx
    try
        let mutable res: SearchProblem array = Array.empty<SearchProblem>
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if i <> idx then
                res <- Array.append res [|(_idx lst (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let _t: int = _now()
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random_float () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (float (rand())) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and randint (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable low = low
    let mutable high = high
    try
        __ret <- ((((rand()) % ((high - low) + 1) + ((high - low) + 1)) % ((high - low) + 1))) + low
        raise Return
        __ret
    with
        | Return -> __ret
and expApprox (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let mutable _y: float = _x
        let mutable is_neg: bool = false
        if _x < 0.0 then
            is_neg <- true
            _y <- -_x
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 30 do
            term <- (term * _y) / (float n)
            sum <- sum + term
            n <- n + 1
        if is_neg then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and simulated_annealing (search_prob: SearchProblem) (f: float -> float -> float) (find_max: bool) (max_x: float) (min_x: float) (max_y: float) (min_y: float) (start_temp: float) (rate_of_decrease: float) (threshold_temp: float) =
    let mutable __ret : SearchProblem = Unchecked.defaultof<SearchProblem>
    let mutable search_prob = search_prob
    let mutable f = f
    let mutable find_max = find_max
    let mutable max_x = max_x
    let mutable min_x = min_x
    let mutable max_y = max_y
    let mutable min_y = min_y
    let mutable start_temp = start_temp
    let mutable rate_of_decrease = rate_of_decrease
    let mutable threshold_temp = threshold_temp
    try
        let mutable search_end: bool = false
        let mutable current_state: SearchProblem = search_prob
        let mutable current_temp: float = start_temp
        let mutable best_state: SearchProblem = current_state
        try
            while not search_end do
                try
                    let current_score: float = score (current_state) (f)
                    if (score (best_state) (f)) < current_score then
                        best_state <- current_state
                    let mutable next_state: SearchProblem = current_state
                    let mutable found_next: bool = false
                    let mutable neighbors: SearchProblem array = get_neighbors (current_state)
                    try
                        while (not found_next) && ((Seq.length (neighbors)) > 0) do
                            try
                                let idx: int = randint (0) ((Seq.length (neighbors)) - 1)
                                let picked_neighbor: SearchProblem = _idx neighbors (int idx)
                                neighbors <- remove_at (neighbors) (idx)
                                if ((((picked_neighbor._x) > max_x) || ((picked_neighbor._x) < min_x)) || ((picked_neighbor._y) > max_y)) || ((picked_neighbor._y) < min_y) then
                                    raise Continue
                                let mutable change: float = (score (picked_neighbor) (f)) - current_score
                                if not find_max then
                                    change <- -change
                                if change > 0.0 then
                                    next_state <- picked_neighbor
                                    found_next <- true
                                else
                                    let probability: float = expApprox (change / current_temp)
                                    if (random_float()) < probability then
                                        next_state <- picked_neighbor
                                        found_next <- true
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    current_temp <- current_temp - (current_temp * rate_of_decrease)
                    if (current_temp < threshold_temp) || (not found_next) then
                        search_end <- true
                    else
                        current_state <- next_state
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- best_state
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
and test_f2 (_x: float) (_y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    let mutable _y = _y
    try
        __ret <- ((3.0 * _x) * _x) - (6.0 * _y)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let prob1: SearchProblem = { _x = 12.0; _y = 47.0; _step = 1.0 }
        let min_state: SearchProblem = simulated_annealing (prob1) (unbox<float -> float -> float> test_f1) (false) (100.0) (5.0) (50.0) (-5.0) (100.0) (0.01) (1.0)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("min1"); sprintf "%g" (test_f1 (min_state._x) (min_state._y))|]))
        let prob2: SearchProblem = { _x = 12.0; _y = 47.0; _step = 1.0 }
        let max_state: SearchProblem = simulated_annealing (prob2) (unbox<float -> float -> float> test_f1) (true) (100.0) (5.0) (50.0) (-5.0) (100.0) (0.01) (1.0)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("max1"); sprintf "%g" (test_f1 (max_state._x) (max_state._y))|]))
        let prob3: SearchProblem = { _x = 3.0; _y = 4.0; _step = 1.0 }
        let min_state2: SearchProblem = simulated_annealing (prob3) (unbox<float -> float -> float> test_f2) (false) (1000.0) (-1000.0) (1000.0) (-1000.0) (100.0) (0.01) (1.0)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("min2"); sprintf "%g" (test_f2 (min_state2._x) (min_state2._y))|]))
        let prob4: SearchProblem = { _x = 3.0; _y = 4.0; _step = 1.0 }
        let max_state2: SearchProblem = simulated_annealing (prob4) (unbox<float -> float -> float> test_f2) (true) (1000.0) (-1000.0) (1000.0) (-1000.0) (100.0) (0.01) (1.0)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("max2"); sprintf "%g" (test_f2 (max_state2._x) (max_state2._y))|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
