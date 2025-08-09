// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
type State = {
    mutable _claim: int array
    mutable _alloc: int array array
    mutable _max: int array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec processes_resource_summation (_alloc: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable _alloc = _alloc
    try
        let resources: int = Seq.length (_idx _alloc (int 0))
        let mutable sums: int array = Array.empty<int>
        let mutable i: int = 0
        while i < resources do
            let mutable total: int = 0
            let mutable j: int = 0
            while j < (Seq.length (_alloc)) do
                total <- total + (_idx (_idx _alloc (int j)) (int i))
                j <- j + 1
            sums <- Array.append sums [|total|]
            i <- i + 1
        __ret <- sums
        raise Return
        __ret
    with
        | Return -> __ret
let rec available_resources (_claim: int array) (alloc_sum: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable _claim = _claim
    let mutable alloc_sum = alloc_sum
    try
        let mutable avail: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (_claim)) do
            avail <- Array.append avail [|((_idx _claim (int i)) - (_idx alloc_sum (int i)))|]
            i <- i + 1
        __ret <- avail
        raise Return
        __ret
    with
        | Return -> __ret
let rec need (_max: int array array) (_alloc: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable _max = _max
    let mutable _alloc = _alloc
    try
        let mutable needs: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (_max)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx _max (int 0))) do
                row <- Array.append row [|((_idx (_idx _max (int i)) (int j)) - (_idx (_idx _alloc (int i)) (int j)))|]
                j <- j + 1
            needs <- Array.append needs [|row|]
            i <- i + 1
        __ret <- needs
        raise Return
        __ret
    with
        | Return -> __ret
let rec pretty_print (_claim: int array) (_alloc: int array array) (_max: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _claim = _claim
    let mutable _alloc = _alloc
    let mutable _max = _max
    try
        printfn "%s" ("         Allocated Resource Table")
        let mutable i: int = 0
        while i < (Seq.length (_alloc)) do
            let mutable row: int array = _idx _alloc (int i)
            let mutable line: string = ("P" + (_str (i + 1))) + "       "
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                line <- line + (_str (_idx row (int j)))
                if j < ((Seq.length (row)) - 1) then
                    line <- line + "        "
                j <- j + 1
            printfn "%s" (line)
            printfn "%s" ("")
            i <- i + 1
        printfn "%s" ("         System Resource Table")
        i <- 0
        while i < (Seq.length (_max)) do
            let mutable row: int array = _idx _max (int i)
            let mutable line: string = ("P" + (_str (i + 1))) + "       "
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                line <- line + (_str (_idx row (int j)))
                if j < ((Seq.length (row)) - 1) then
                    line <- line + "        "
                j <- j + 1
            printfn "%s" (line)
            printfn "%s" ("")
            i <- i + 1
        let mutable usage: string = ""
        i <- 0
        while i < (Seq.length (_claim)) do
            if i > 0 then
                usage <- usage + " "
            usage <- usage + (_str (_idx _claim (int i)))
            i <- i + 1
        let mutable alloc_sum: int array = processes_resource_summation (_alloc)
        let mutable avail: int array = available_resources (_claim) (alloc_sum)
        let mutable avail_str: string = ""
        i <- 0
        while i < (Seq.length (avail)) do
            if i > 0 then
                avail_str <- avail_str + " "
            avail_str <- avail_str + (_str (_idx avail (int i)))
            i <- i + 1
        printfn "%s" ("Current Usage by Active Processes: " + usage)
        printfn "%s" ("Initial Available Resources:       " + avail_str)
        __ret
    with
        | Return -> __ret
let rec bankers_algorithm (_claim: int array) (_alloc: int array array) (_max: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _claim = _claim
    let mutable _alloc = _alloc
    let mutable _max = _max
    try
        let mutable need_list: int array array = need (_max) (_alloc)
        let mutable alloc_sum: int array = processes_resource_summation (_alloc)
        let mutable avail: int array = available_resources (_claim) (alloc_sum)
        printfn "%s" ("__________________________________________________")
        printfn "%s" ("")
        let mutable finished: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < (Seq.length (need_list)) do
            finished <- Array.append finished [|false|]
            i <- i + 1
        let mutable remaining: int = Seq.length (need_list)
        try
            while remaining > 0 do
                try
                    let mutable safe: bool = false
                    let mutable p: int = 0
                    try
                        while p < (Seq.length (need_list)) do
                            try
                                if not (_idx finished (int p)) then
                                    let mutable exec: bool = true
                                    let mutable r: int = 0
                                    try
                                        while r < (Seq.length (avail)) do
                                            try
                                                if (_idx (_idx need_list (int p)) (int r)) > (_idx avail (int r)) then
                                                    exec <- false
                                                    raise Break
                                                r <- r + 1
                                            with
                                            | Continue -> ()
                                            | Break -> raise Break
                                    with
                                    | Break -> ()
                                    | Continue -> ()
                                    if exec then
                                        safe <- true
                                        printfn "%s" (("Process " + (_str (p + 1))) + " is executing.")
                                        r <- 0
                                        while r < (Seq.length (avail)) do
                                            avail.[int r] <- (_idx avail (int r)) + (_idx (_idx _alloc (int p)) (int r))
                                            r <- r + 1
                                        let mutable avail_str: string = ""
                                        r <- 0
                                        while r < (Seq.length (avail)) do
                                            if r > 0 then
                                                avail_str <- avail_str + " "
                                            avail_str <- avail_str + (_str (_idx avail (int r)))
                                            r <- r + 1
                                        printfn "%s" ("Updated available resource stack for processes: " + avail_str)
                                        printfn "%s" ("The process is in a safe state.")
                                        printfn "%s" ("")
                                        finished.[int p] <- true
                                        remaining <- remaining - 1
                                p <- p + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not safe then
                        printfn "%s" ("System in unsafe state. Aborting...")
                        printfn "%s" ("")
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
let mutable claim_vector: int array = unbox<int array> [|8; 5; 9; 7|]
let mutable allocated_resources_table: int array array = [|[|2; 0; 1; 1|]; [|0; 1; 2; 1|]; [|4; 0; 0; 3|]; [|0; 2; 1; 0|]; [|1; 0; 3; 0|]|]
let mutable maximum_claim_table: int array array = [|[|3; 2; 1; 4|]; [|0; 2; 5; 2|]; [|5; 1; 0; 5|]; [|1; 5; 3; 0|]; [|3; 0; 3; 3|]|]
pretty_print (claim_vector) (allocated_resources_table) (maximum_claim_table)
bankers_algorithm (claim_vector) (allocated_resources_table) (maximum_claim_table)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
