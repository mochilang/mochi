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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Process = {
    mutable _process_name: string
    mutable _arrival_time: int
    mutable _stop_time: int
    mutable _burst_time: int
    mutable _waiting_time: int
    mutable _turnaround_time: int
}
type MLFQ = {
    mutable _number_of_queues: int
    mutable _time_slices: int array
    mutable _ready_queue: Process array
    mutable _current_time: int
    mutable _finish_queue: Process array
}
type RRResult = {
    mutable _finished: Process array
    mutable _ready: Process array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_process (name: string) (arrival: int) (burst: int) =
    let mutable __ret : Process = Unchecked.defaultof<Process>
    let mutable name = name
    let mutable arrival = arrival
    let mutable burst = burst
    try
        __ret <- { _process_name = name; _arrival_time = arrival; _stop_time = arrival; _burst_time = burst; _waiting_time = 0; _turnaround_time = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_mlfq (nqueues: int) (_time_slices: int array) (queue: Process array) (_current_time: int) =
    let mutable __ret : MLFQ = Unchecked.defaultof<MLFQ>
    let mutable nqueues = nqueues
    let mutable _time_slices = _time_slices
    let mutable queue = queue
    let mutable _current_time = _current_time
    try
        __ret <- { _number_of_queues = nqueues; _time_slices = _time_slices; _ready_queue = queue; _current_time = _current_time; _finish_queue = Array.empty<Process> }
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_sequence_of_finish_queue (mlfq: MLFQ) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable mlfq = mlfq
    try
        let mutable seq: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (mlfq._finish_queue)) do
            let p: Process = _idx (mlfq._finish_queue) (int i)
            seq <- Array.append seq [|(p._process_name)|]
            i <- i + 1
        __ret <- seq
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_waiting_time (queue: Process array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable queue = queue
    try
        let mutable times: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (queue)) do
            let p: Process = _idx queue (int i)
            times <- Array.append times [|(p._waiting_time)|]
            i <- i + 1
        __ret <- times
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_turnaround_time (queue: Process array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable queue = queue
    try
        let mutable times: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (queue)) do
            let p: Process = _idx queue (int i)
            times <- Array.append times [|(p._turnaround_time)|]
            i <- i + 1
        __ret <- times
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_completion_time (queue: Process array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable queue = queue
    try
        let mutable times: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (queue)) do
            let p: Process = _idx queue (int i)
            times <- Array.append times [|(p._stop_time)|]
            i <- i + 1
        __ret <- times
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_remaining_burst_time_of_processes (queue: Process array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable queue = queue
    try
        let mutable times: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (queue)) do
            let p: Process = _idx queue (int i)
            times <- Array.append times [|(p._burst_time)|]
            i <- i + 1
        __ret <- times
        raise Return
        __ret
    with
        | Return -> __ret
let rec update_waiting_time (mlfq: MLFQ) (process: Process) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable mlfq = mlfq
    let mutable process = process
    try
        process._waiting_time <- (process._waiting_time) + ((mlfq._current_time) - (process._stop_time))
        __ret <- process._waiting_time
        raise Return
        __ret
    with
        | Return -> __ret
let rec first_come_first_served (mlfq: MLFQ) (_ready_queue: Process array) =
    let mutable __ret : Process array = Unchecked.defaultof<Process array>
    let mutable mlfq = mlfq
    let mutable _ready_queue = _ready_queue
    try
        let mutable _finished: Process array = Array.empty<Process>
        let mutable rq: Process array = _ready_queue
        while (Seq.length (rq)) <> 0 do
            let cp: Process = _idx rq (int 0)
            rq <- Array.sub rq 1 ((Seq.length (rq)) - 1)
            if (mlfq._current_time) < (cp._arrival_time) then
                mlfq._current_time <- cp._arrival_time
            update_waiting_time (mlfq) (cp)
            mlfq._current_time <- (mlfq._current_time) + (cp._burst_time)
            cp._burst_time <- 0
            cp._turnaround_time <- (mlfq._current_time) - (cp._arrival_time)
            cp._stop_time <- mlfq._current_time
            _finished <- Array.append _finished [|cp|]
        mlfq._finish_queue <- Array.append (mlfq._finish_queue) (_finished)
        __ret <- _finished
        raise Return
        __ret
    with
        | Return -> __ret
let rec round_robin (mlfq: MLFQ) (_ready_queue: Process array) (time_slice: int) =
    let mutable __ret : RRResult = Unchecked.defaultof<RRResult>
    let mutable mlfq = mlfq
    let mutable _ready_queue = _ready_queue
    let mutable time_slice = time_slice
    try
        let mutable _finished: Process array = Array.empty<Process>
        let mutable rq: Process array = _ready_queue
        let mutable count: int = Seq.length (rq)
        let mutable i: int = 0
        while i < count do
            let cp: Process = _idx rq (int 0)
            rq <- Array.sub rq 1 ((Seq.length (rq)) - 1)
            if (mlfq._current_time) < (cp._arrival_time) then
                mlfq._current_time <- cp._arrival_time
            update_waiting_time (mlfq) (cp)
            if (cp._burst_time) > time_slice then
                mlfq._current_time <- (mlfq._current_time) + time_slice
                cp._burst_time <- (cp._burst_time) - time_slice
                cp._stop_time <- mlfq._current_time
                rq <- Array.append rq [|cp|]
            else
                mlfq._current_time <- (mlfq._current_time) + (cp._burst_time)
                cp._burst_time <- 0
                cp._stop_time <- mlfq._current_time
                cp._turnaround_time <- (mlfq._current_time) - (cp._arrival_time)
                _finished <- Array.append _finished [|cp|]
            i <- i + 1
        mlfq._finish_queue <- Array.append (mlfq._finish_queue) (_finished)
        __ret <- { _finished = _finished; _ready = rq }
        raise Return
        __ret
    with
        | Return -> __ret
let rec multi_level_feedback_queue (mlfq: MLFQ) =
    let mutable __ret : Process array = Unchecked.defaultof<Process array>
    let mutable mlfq = mlfq
    try
        let mutable i: int = 0
        while i < ((mlfq._number_of_queues) - 1) do
            let rr: RRResult = round_robin (mlfq) (mlfq._ready_queue) (_idx (mlfq._time_slices) (int i))
            mlfq._ready_queue <- rr._ready
            i <- i + 1
        first_come_first_served (mlfq) (mlfq._ready_queue)
        __ret <- mlfq._finish_queue
        raise Return
        __ret
    with
        | Return -> __ret
let P1: Process = make_process ("P1") (0) (53)
let P2: Process = make_process ("P2") (0) (17)
let P3: Process = make_process ("P3") (0) (68)
let P4: Process = make_process ("P4") (0) (24)
let _number_of_queues: int = 3
let _time_slices: int array = unbox<int array> [|17; 25|]
let queue: Process array = unbox<Process array> [|P1; P2; P3; P4|]
let mlfq: MLFQ = make_mlfq (_number_of_queues) (_time_slices) (queue) (0)
let _finish_queue: Process array = multi_level_feedback_queue (mlfq)
printfn "%s" ("waiting time:\t\t\t" + (_str (calculate_waiting_time (unbox<Process array> [|P1; P2; P3; P4|]))))
printfn "%s" ("completion time:\t\t" + (_str (calculate_completion_time (unbox<Process array> [|P1; P2; P3; P4|]))))
printfn "%s" ("turnaround time:\t\t" + (_str (calculate_turnaround_time (unbox<Process array> [|P1; P2; P3; P4|]))))
printfn "%s" ("sequence of finished processes:\t" + (_str (calculate_sequence_of_finish_queue (mlfq))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
