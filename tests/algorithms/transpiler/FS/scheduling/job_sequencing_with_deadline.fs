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
type Job = {
    mutable _id: int
    mutable _deadline: int
    mutable _profit: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sort_jobs_by_profit (jobs: Job array) =
    let mutable __ret : Job array = Unchecked.defaultof<Job array>
    let mutable jobs = jobs
    try
        let mutable js: Job array = jobs
        let mutable i: int = 0
        while i < (Seq.length (js)) do
            let mutable j: int = 0
            while j < (((Seq.length (js)) - i) - 1) do
                let a: Job = _idx js (int j)
                let b: Job = _idx js (int (j + 1))
                if (a._profit) < (b._profit) then
                    js.[int j] <- b
                    js.[int (j + 1)] <- a
                j <- j + 1
            i <- i + 1
        __ret <- js
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_deadline (jobs: Job array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable jobs = jobs
    try
        let mutable max_d: int = 0
        let mutable i: int = 0
        while i < (Seq.length (jobs)) do
            let job: Job = _idx jobs (int i)
            let d: int = job._deadline
            if d > max_d then
                max_d <- d
            i <- i + 1
        __ret <- max_d
        raise Return
        __ret
    with
        | Return -> __ret
let rec job_sequencing_with_deadlines (jobs: Job array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable jobs = jobs
    try
        let mutable js: Job array = sort_jobs_by_profit (jobs)
        let mutable max_d: int = max_deadline (js)
        let mutable time_slots: int array = Array.empty<int>
        let mutable t: int = 0
        while t < max_d do
            time_slots <- Array.append time_slots [|(0 - 1)|]
            t <- t + 1
        let mutable count: int = 0
        let mutable max_profit: int = 0
        let mutable i: int = 0
        try
            while i < (Seq.length (js)) do
                try
                    let job: Job = _idx js (int i)
                    let mutable j: int = (job._deadline) - 1
                    try
                        while j >= 0 do
                            try
                                if (_idx time_slots (int j)) = (0 - 1) then
                                    time_slots.[int j] <- job._id
                                    count <- count + 1
                                    max_profit <- max_profit + (job._profit)
                                    raise Break
                                j <- j - 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable result: int array = Array.empty<int>
        result <- Array.append result [|count|]
        result <- Array.append result [|max_profit|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let mutable jobs1: Job array = Array.empty<Job>
jobs1 <- Array.append jobs1 [|{ _id = 1; _deadline = 4; _profit = 20 }|]
jobs1 <- Array.append jobs1 [|{ _id = 2; _deadline = 1; _profit = 10 }|]
jobs1 <- Array.append jobs1 [|{ _id = 3; _deadline = 1; _profit = 40 }|]
jobs1 <- Array.append jobs1 [|{ _id = 4; _deadline = 1; _profit = 30 }|]
printfn "%s" (_str (job_sequencing_with_deadlines (jobs1)))
let mutable jobs2: Job array = Array.empty<Job>
jobs2 <- Array.append jobs2 [|{ _id = 1; _deadline = 2; _profit = 100 }|]
jobs2 <- Array.append jobs2 [|{ _id = 2; _deadline = 1; _profit = 19 }|]
jobs2 <- Array.append jobs2 [|{ _id = 3; _deadline = 2; _profit = 27 }|]
jobs2 <- Array.append jobs2 [|{ _id = 4; _deadline = 1; _profit = 25 }|]
jobs2 <- Array.append jobs2 [|{ _id = 5; _deadline = 1; _profit = 15 }|]
printfn "%s" (_str (job_sequencing_with_deadlines (jobs2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
