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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec calculate_waitingtime (arrival_time: int array) (burst_time: int array) (no_of_processes: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arrival_time = arrival_time
    let mutable burst_time = burst_time
    let mutable no_of_processes = no_of_processes
    try
        let mutable remaining_time: int array = Array.empty<int>
        let mutable i: int = 0
        while i < no_of_processes do
            remaining_time <- Array.append remaining_time [|(_idx burst_time (int i))|]
            i <- i + 1
        let mutable waiting_time: int array = Array.empty<int>
        i <- 0
        while i < no_of_processes do
            waiting_time <- Array.append waiting_time [|0|]
            i <- i + 1
        let mutable complete: int = 0
        let mutable increment_time: int = 0
        let mutable minm: int = 1000000000
        let mutable short: int = 0
        let mutable check: bool = false
        try
            while complete <> no_of_processes do
                try
                    let mutable j: int = 0
                    while j < no_of_processes do
                        if (((_idx arrival_time (int j)) <= increment_time) && ((_idx remaining_time (int j)) > 0)) && ((_idx remaining_time (int j)) < minm) then
                            minm <- _idx remaining_time (int j)
                            short <- j
                            check <- true
                        j <- j + 1
                    if not check then
                        increment_time <- increment_time + 1
                        raise Continue
                    remaining_time.[int short] <- (_idx remaining_time (int short)) - 1
                    minm <- _idx remaining_time (int short)
                    if minm = 0 then
                        minm <- 1000000000
                    if (_idx remaining_time (int short)) = 0 then
                        complete <- complete + 1
                        check <- false
                        let finish_time: int = increment_time + 1
                        let finar: int = finish_time - (_idx arrival_time (int short))
                        waiting_time.[int short] <- finar - (_idx burst_time (int short))
                        if (_idx waiting_time (int short)) < 0 then
                            waiting_time.[int short] <- 0
                    increment_time <- increment_time + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- waiting_time
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_turnaroundtime (burst_time: int array) (no_of_processes: int) (waiting_time: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable burst_time = burst_time
    let mutable no_of_processes = no_of_processes
    let mutable waiting_time = waiting_time
    try
        let mutable turn_around_time: int array = Array.empty<int>
        let mutable i: int = 0
        while i < no_of_processes do
            turn_around_time <- Array.append turn_around_time [|((_idx burst_time (int i)) + (_idx waiting_time (int i)))|]
            i <- i + 1
        __ret <- turn_around_time
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_average_times (waiting_time: int array) (turn_around_time: int array) (no_of_processes: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable waiting_time = waiting_time
    let mutable turn_around_time = turn_around_time
    let mutable no_of_processes = no_of_processes
    try
        let mutable total_waiting_time: int = 0
        let mutable total_turn_around_time: int = 0
        let mutable i: int = 0
        while i < no_of_processes do
            total_waiting_time <- total_waiting_time + (_idx waiting_time (int i))
            total_turn_around_time <- total_turn_around_time + (_idx turn_around_time (int i))
            i <- i + 1
        let avg_wait: float = (to_float (total_waiting_time)) / (to_float (no_of_processes))
        let avg_turn: float = (to_float (total_turn_around_time)) / (to_float (no_of_processes))
        printfn "%s" ("Average waiting time = " + (_str (avg_wait)))
        printfn "%s" ("Average turn around time = " + (_str (avg_turn)))
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (calculate_waitingtime (unbox<int array> [|1; 2; 3; 4|]) (unbox<int array> [|3; 3; 5; 1|]) (4)))
printfn "%s" (_repr (calculate_waitingtime (unbox<int array> [|1; 2; 3|]) (unbox<int array> [|2; 5; 1|]) (3)))
printfn "%s" (_repr (calculate_waitingtime (unbox<int array> [|2; 3|]) (unbox<int array> [|5; 1|]) (2)))
printfn "%s" (_repr (calculate_turnaroundtime (unbox<int array> [|3; 3; 5; 1|]) (4) (unbox<int array> [|0; 3; 5; 0|])))
printfn "%s" (_repr (calculate_turnaroundtime (unbox<int array> [|3; 3|]) (2) (unbox<int array> [|0; 3|])))
printfn "%s" (_repr (calculate_turnaroundtime (unbox<int array> [|8; 10; 1|]) (3) (unbox<int array> [|1; 0; 3|])))
calculate_average_times (unbox<int array> [|0; 3; 5; 0|]) (unbox<int array> [|3; 6; 10; 1|]) (4)
calculate_average_times (unbox<int array> [|2; 3|]) (unbox<int array> [|3; 6|]) (2)
calculate_average_times (unbox<int array> [|10; 4; 3|]) (unbox<int array> [|2; 7; 6|]) (3)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
