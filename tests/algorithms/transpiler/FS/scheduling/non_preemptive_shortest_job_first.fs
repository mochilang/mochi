// Generated 2025-08-11 16:20 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec calculate_waitingtime (arrival_time: int array) (burst_time: int array) (no_of_processes: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arrival_time = arrival_time
    let mutable burst_time = burst_time
    let mutable no_of_processes = no_of_processes
    try
        let mutable waiting_time: int array = Array.empty<int>
        let mutable remaining_time: int array = Array.empty<int>
        let mutable i: int = 0
        while i < no_of_processes do
            waiting_time <- Array.append waiting_time [|0|]
            remaining_time <- Array.append remaining_time [|(_idx burst_time (int i))|]
            i <- i + 1
        let mutable completed: int = 0
        let mutable total_time: int = 0
        while completed <> no_of_processes do
            let mutable ready_process: int array = Array.empty<int>
            let mutable target_process: int = -1
            let mutable j: int = 0
            while j < no_of_processes do
                if ((_idx arrival_time (int j)) <= total_time) && ((_idx remaining_time (int j)) > 0) then
                    ready_process <- Array.append ready_process [|j|]
                j <- j + 1
            if (Seq.length (ready_process)) > 0 then
                target_process <- _idx ready_process (int 0)
                let mutable k: int = 0
                while k < (Seq.length (ready_process)) do
                    let idx: int = _idx ready_process (int k)
                    if (_idx remaining_time (int idx)) < (_idx remaining_time (int target_process)) then
                        target_process <- idx
                    k <- k + 1
                total_time <- total_time + (_idx burst_time (int target_process))
                completed <- completed + 1
                remaining_time.[int target_process] <- 0
                waiting_time <- _arrset waiting_time (int target_process) ((total_time - (_idx arrival_time (int target_process))) - (_idx burst_time (int target_process)))
            else
                total_time <- total_time + 1
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
let rec average (values: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable values = values
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            total <- total + (_idx values (int i))
            i <- i + 1
        __ret <- (float total) / (float (Seq.length (values)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("[TEST CASE 01]")
let no_of_processes: int = 4
let burst_time: int array = unbox<int array> [|2; 5; 3; 7|]
let arrival_time: int array = unbox<int array> [|0; 0; 0; 0|]
let mutable waiting_time: int array = calculate_waitingtime (arrival_time) (burst_time) (no_of_processes)
let mutable turn_around_time: int array = calculate_turnaroundtime (burst_time) (no_of_processes) (waiting_time)
printfn "%s" ("PID\tBurst Time\tArrival Time\tWaiting Time\tTurnaround Time")
let mutable i: int = 0
while i < no_of_processes do
    let pid: int = i + 1
    printfn "%s" (((((((((_str (pid)) + "\t") + (_str (_idx burst_time (int i)))) + "\t\t\t") + (_str (_idx arrival_time (int i)))) + "\t\t\t\t") + (_str (_idx waiting_time (int i)))) + "\t\t\t\t") + (_str (_idx turn_around_time (int i))))
    i <- i + 1
let avg_wait: float = average (waiting_time)
let avg_turn: float = average (turn_around_time)
printfn "%s" ("\nAverage waiting time = " + (_str (avg_wait)))
printfn "%s" ("Average turnaround time = " + (_str (avg_turn)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
