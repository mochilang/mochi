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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec calculate_waiting_times (burst_times: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable burst_times = burst_times
    try
        let quantum: int = 2
        let mutable rem: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (burst_times)) do
            rem <- Array.append rem [|(_idx burst_times (int i))|]
            i <- i + 1
        let mutable waiting: int array = Array.empty<int>
        i <- 0
        while i < (Seq.length (burst_times)) do
            waiting <- Array.append waiting [|0|]
            i <- i + 1
        let mutable t: int = 0
        while true do
            let mutable ``done``: bool = true
            let mutable j: int = 0
            while j < (Seq.length (burst_times)) do
                if (_idx rem (int j)) > 0 then
                    ``done`` <- false
                    if (_idx rem (int j)) > quantum then
                        t <- t + quantum
                        rem.[int j] <- (_idx rem (int j)) - quantum
                    else
                        t <- t + (_idx rem (int j))
                        waiting.[int j] <- t - (_idx burst_times (int j))
                        rem.[int j] <- 0
                j <- j + 1
            if ``done`` then
                __ret <- waiting
                raise Return
        __ret <- waiting
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_turn_around_times (burst_times: int array) (waiting_times: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable burst_times = burst_times
    let mutable waiting_times = waiting_times
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (burst_times)) do
            result <- Array.append result [|((_idx burst_times (int i)) + (_idx waiting_times (int i)))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and mean (values: int array) =
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
and format_float_5 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let scaled: int = int ((x * 100000.0) + 0.5)
        let int_part: int = _floordiv scaled 100000
        let frac_part: int = ((scaled % 100000 + 100000) % 100000)
        let mutable frac_str: string = _str (frac_part)
        while (String.length (frac_str)) < 5 do
            frac_str <- "0" + frac_str
        __ret <- ((_str (int_part)) + ".") + frac_str
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let burst_times: int array = unbox<int array> [|3; 5; 7|]
        let waiting_times: int array = calculate_waiting_times (burst_times)
        let turn_around_times: int array = calculate_turn_around_times (burst_times) (waiting_times)
        printfn "%s" ("Process ID \tBurst Time \tWaiting Time \tTurnaround Time")
        let mutable i: int = 0
        while i < (Seq.length (burst_times)) do
            let line: string = (((((("  " + (_str (i + 1))) + "\t\t  ") + (_str (_idx burst_times (int i)))) + "\t\t  ") + (_str (_idx waiting_times (int i)))) + "\t\t  ") + (_str (_idx turn_around_times (int i)))
            printfn "%s" (line)
            i <- i + 1
        printfn "%s" ("")
        printfn "%s" ("Average waiting time = " + (format_float_5 (mean (waiting_times))))
        printfn "%s" ("Average turn around time = " + (format_float_5 (mean (turn_around_times))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
