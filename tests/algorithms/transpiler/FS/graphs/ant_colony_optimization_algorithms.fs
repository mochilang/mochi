// Generated 2025-08-07 16:27 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
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
open System.Collections.Generic

open System

let rec sqrtApprox (x: float) =
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
let rec rand_float () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (float ((((_now()) % 1000000 + 1000000) % 1000000))) / 1000000.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_float (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        let e: int = int exp
        while i < e do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec distance (city1: int array) (city2: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable city1 = city1
    let mutable city2 = city2
    try
        let dx: float = float ((_idx city1 (0)) - (_idx city2 (0)))
        let dy: float = float ((_idx city1 (1)) - (_idx city2 (1)))
        __ret <- sqrtApprox ((dx * dx) + (dy * dy))
        raise Return
        __ret
    with
        | Return -> __ret
let rec choose_weighted (options: int array) (weights: float array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable options = options
    let mutable weights = weights
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (weights)) do
            total <- total + (_idx weights (i))
            i <- i + 1
        let mutable r: float = (rand_float()) * total
        let mutable accum: float = 0.0
        i <- 0
        while i < (Seq.length (weights)) do
            accum <- accum + (_idx weights (i))
            if r <= accum then
                __ret <- _idx options (i)
                raise Return
            i <- i + 1
        __ret <- _idx options ((Seq.length (options)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec city_select (pheromone: float array array) (current: int) (unvisited: int array) (alpha: float) (beta: float) (cities: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable pheromone = pheromone
    let mutable current = current
    let mutable unvisited = unvisited
    let mutable alpha = alpha
    let mutable beta = beta
    let mutable cities = cities
    try
        let mutable probs: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (unvisited)) do
            let city: int = _idx unvisited (i)
            let mutable dist: float = distance (cities.[city]) (cities.[current])
            let trail: float = _idx (_idx pheromone (city)) (current)
            let prob: float = (pow_float (trail) (alpha)) * (pow_float (1.0 / dist) (beta))
            probs <- Array.append probs [|prob|]
            i <- i + 1
        __ret <- choose_weighted (unvisited) (probs)
        raise Return
        __ret
    with
        | Return -> __ret
let rec pheromone_update (pheromone: float array array) (cities: System.Collections.Generic.IDictionary<int, int array>) (evaporation: float) (ants_route: int array array) (q: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable pheromone = pheromone
    let mutable cities = cities
    let mutable evaporation = evaporation
    let mutable ants_route = ants_route
    let mutable q = q
    try
        let n: int = Seq.length (pheromone)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < n do
                pheromone.[i].[j] <- (_idx (_idx pheromone (i)) (j)) * evaporation
                j <- j + 1
            i <- i + 1
        let mutable a: int = 0
        while a < (Seq.length (ants_route)) do
            let mutable route: int array = _idx ants_route (a)
            let mutable total: float = 0.0
            let mutable r: int = 0
            while r < ((Seq.length (route)) - 1) do
                total <- total + (distance (cities.[_idx route (r)]) (cities.[_idx route (r + 1)]))
                r <- r + 1
            let delta: float = q / total
            r <- 0
            while r < ((Seq.length (route)) - 1) do
                let u: int = _idx route (r)
                let v: int = _idx route (r + 1)
                pheromone.[u].[v] <- (_idx (_idx pheromone (u)) (v)) + delta
                pheromone.[v].[u] <- _idx (_idx pheromone (u)) (v)
                r <- r + 1
            a <- a + 1
        __ret <- pheromone
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_value (lst: int array) (``val``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable lst = lst
    let mutable ``val`` = ``val``
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) <> ``val`` then
                res <- Array.append res [|(_idx lst (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec ant_colony (cities: System.Collections.Generic.IDictionary<int, int array>) (ants_num: int) (iterations: int) (evaporation: float) (alpha: float) (beta: float) (q: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable cities = cities
    let mutable ants_num = ants_num
    let mutable iterations = iterations
    let mutable evaporation = evaporation
    let mutable alpha = alpha
    let mutable beta = beta
    let mutable q = q
    try
        let n: int = Seq.length (cities)
        let mutable pheromone: float array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|1.0|]
                j <- j + 1
            pheromone <- Array.append pheromone [|row|]
            i <- i + 1
        let mutable best_path: int array = [||]
        let mutable best_distance: float = 1000000000.0
        let mutable iter: int = 0
        while iter < iterations do
            let mutable ants_route: int array array = [||]
            let mutable k: int = 0
            while k < ants_num do
                let mutable route: int array = [|0|]
                let mutable unvisited: int array = [||]
                for key in cities.Keys do
                    if key <> 0 then
                        unvisited <- Array.append unvisited [|key|]
                let mutable current: int = 0
                while (Seq.length (unvisited)) > 0 do
                    let next_city: int = city_select (pheromone) (current) (unvisited) (alpha) (beta) (cities)
                    route <- Array.append route [|next_city|]
                    unvisited <- remove_value (unvisited) (next_city)
                    current <- next_city
                route <- Array.append route [|0|]
                ants_route <- Array.append ants_route [|route|]
                k <- k + 1
            pheromone <- pheromone_update (pheromone) (cities) (evaporation) (ants_route) (q)
            let mutable a: int = 0
            while a < (Seq.length (ants_route)) do
                let mutable route: int array = _idx ants_route (a)
                let mutable dist: float = 0.0
                let mutable r: int = 0
                while r < ((Seq.length (route)) - 1) do
                    dist <- dist + (distance (cities.[_idx route (r)]) (cities.[_idx route (r + 1)]))
                    r <- r + 1
                if dist < best_distance then
                    best_distance <- dist
                    best_path <- route
                a <- a + 1
            iter <- iter + 1
        printfn "%s" ("best_path = " + (_str (best_path)))
        printfn "%s" ("best_distance = " + (_str (best_distance)))
        __ret
    with
        | Return -> __ret
let cities: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|0; 0|]); (1, [|0; 5|]); (2, [|3; 8|]); (3, [|8; 10|]); (4, [|12; 8|]); (5, [|12; 4|]); (6, [|8; 0|]); (7, [|6; 2|])]
ant_colony (cities) (10) (20) (0.7) (1.0) (5.0) (10.0)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
