// Generated 2025-08-12 07:47 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec josephus_recursive (num_people: int) (step_size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num_people = num_people
    let mutable step_size = step_size
    try
        if (num_people <= 0) || (step_size <= 0) then
            failwith ("num_people or step_size is not a positive integer.")
        if num_people = 1 then
            __ret <- 0
            raise Return
        __ret <- ((((josephus_recursive (num_people - 1) (step_size)) + step_size) % num_people + num_people) % num_people)
        raise Return
        __ret
    with
        | Return -> __ret
and find_winner (num_people: int) (step_size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num_people = num_people
    let mutable step_size = step_size
    try
        __ret <- (josephus_recursive (num_people) (step_size)) + 1
        raise Return
        __ret
    with
        | Return -> __ret
and remove_at (xs: int array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and josephus_iterative (num_people: int) (step_size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num_people = num_people
    let mutable step_size = step_size
    try
        if (num_people <= 0) || (step_size <= 0) then
            failwith ("num_people or step_size is not a positive integer.")
        let mutable circle: int array = Array.empty<int>
        let mutable i: int = 1
        while i <= num_people do
            circle <- Array.append circle [|i|]
            i <- i + 1
        let mutable current: int = 0
        while (Seq.length (circle)) > 1 do
            current <- ((((current + step_size) - 1) % (Seq.length (circle)) + (Seq.length (circle))) % (Seq.length (circle)))
            circle <- remove_at (circle) (current)
        __ret <- _idx circle (int 0)
        raise Return
        __ret
    with
        | Return -> __ret
let r: int = josephus_recursive (7) (3)
printfn "%s" (_str (r))
printfn "%s" (_str (find_winner (7) (3)))
printfn "%s" (_str (josephus_iterative (7) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
