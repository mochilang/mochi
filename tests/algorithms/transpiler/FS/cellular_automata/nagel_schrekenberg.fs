// Generated 2025-08-06 22:14 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let mutable seed: int = 1
let NEG_ONE: int = -1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        seed <- int ((((int64 ((seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- seed
        raise Return
        __ret
    with
        | Return -> __ret
and randint (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let r: int = rand()
        __ret <- a + (((r % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and construct_highway (number_of_cells: int) (frequency: int) (initial_speed: int) (random_frequency: bool) (random_speed: bool) (max_speed: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable number_of_cells = number_of_cells
    let mutable frequency = frequency
    let mutable initial_speed = initial_speed
    let mutable random_frequency = random_frequency
    let mutable random_speed = random_speed
    let mutable max_speed = max_speed
    try
        let mutable row: int array = [||]
        let mutable i: int = 0
        while i < number_of_cells do
            row <- Array.append row [|-1|]
            i <- i + 1
        let mutable highway: int array array = [||]
        highway <- Array.append highway [|row|]
        i <- 0
        if initial_speed < 0 then
            initial_speed <- 0
        while i < number_of_cells do
            let mutable speed: int = initial_speed
            if random_speed then
                speed <- randint (0) (max_speed)
            highway.[0].[i] <- speed
            let mutable step: int = frequency
            if random_frequency then
                step <- randint (1) (max_speed * 2)
            i <- i + step
        __ret <- highway
        raise Return
        __ret
    with
        | Return -> __ret
and get_distance (highway_now: int array) (car_index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable highway_now = highway_now
    let mutable car_index = car_index
    try
        let mutable distance: int = 0
        let mutable i: int = car_index + 1
        while i < (Seq.length (highway_now)) do
            if (_idx highway_now (i)) > NEG_ONE then
                __ret <- distance
                raise Return
            distance <- distance + 1
            i <- i + 1
        __ret <- distance + (get_distance (highway_now) (-1))
        raise Return
        __ret
    with
        | Return -> __ret
and update (highway_now: int array) (probability: float) (max_speed: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable highway_now = highway_now
    let mutable probability = probability
    let mutable max_speed = max_speed
    try
        let number_of_cells: int = Seq.length (highway_now)
        let mutable next_highway: int array = [||]
        let mutable i: int = 0
        while i < number_of_cells do
            next_highway <- Array.append next_highway [|-1|]
            i <- i + 1
        let mutable car_index: int = 0
        while car_index < number_of_cells do
            let mutable speed: int = _idx highway_now (car_index)
            if speed > NEG_ONE then
                let mutable new_speed: int = speed + 1
                if new_speed > max_speed then
                    new_speed <- max_speed
                let dn: int = (get_distance (highway_now) (car_index)) - 1
                if new_speed > dn then
                    new_speed <- dn
                if (random()) < probability then
                    new_speed <- new_speed - 1
                    if new_speed < 0 then
                        new_speed <- 0
                next_highway <- _arrset next_highway car_index new_speed
            car_index <- car_index + 1
        __ret <- next_highway
        raise Return
        __ret
    with
        | Return -> __ret
and simulate (highway: int array array) (number_of_update: int) (probability: float) (max_speed: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable highway = highway
    let mutable number_of_update = number_of_update
    let mutable probability = probability
    let mutable max_speed = max_speed
    try
        let number_of_cells: int = Seq.length (_idx highway (0))
        let mutable i: int = 0
        while i < number_of_update do
            let next_speeds: int array = update (_idx highway (i)) (probability) (max_speed)
            let mutable real_next: int array = [||]
            let mutable j: int = 0
            while j < number_of_cells do
                real_next <- Array.append real_next [|-1|]
                j <- j + 1
            let mutable k: int = 0
            while k < number_of_cells do
                let mutable speed: int = _idx next_speeds (k)
                if speed > NEG_ONE then
                    let index: int = (((k + speed) % number_of_cells + number_of_cells) % number_of_cells)
                    real_next <- _arrset real_next index speed
                k <- k + 1
            highway <- Array.append highway [|real_next|]
            i <- i + 1
        __ret <- highway
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ex1: int array array = simulate (construct_highway (6) (3) (0) (false) (false) (2)) (2) (0.0) (2)
        printfn "%s" (_str (ex1))
        let ex2: int array array = simulate (construct_highway (5) (2) (-2) (false) (false) (2)) (3) (0.0) (2)
        printfn "%s" (_str (ex2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
