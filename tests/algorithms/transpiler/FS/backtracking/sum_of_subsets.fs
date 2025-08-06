// Generated 2025-08-06 21:04 +0700

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
let json (arr:int array array) =
    printf "[\n"
    for i in 0 .. arr.Length - 1 do
        let line = String.concat ", " (Array.map string arr.[i] |> Array.toList)
        if i < arr.Length - 1 then
            printfn "  [%s]," line
        else
            printfn "  [%s]" line
    printfn "]"
let rec sum_list (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        let mutable s: int = 0
        for n in nums do
            s <- s + n
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and create_state_space_tree (nums: int array) (max_sum: int) (num_index: int) (path: int array) (curr_sum: int) (remaining_sum: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    let mutable max_sum = max_sum
    let mutable num_index = num_index
    let mutable path = path
    let mutable curr_sum = curr_sum
    let mutable remaining_sum = remaining_sum
    try
        let mutable result: int array array = [||]
        if (curr_sum > max_sum) || ((curr_sum + remaining_sum) < max_sum) then
            __ret <- result
            raise Return
        if curr_sum = max_sum then
            result <- Array.append result [|path|]
            __ret <- result
            raise Return
        let mutable index: int = num_index
        while index < (Seq.length (nums)) do
            let value: int = _idx nums (index)
            let subres: int array array = create_state_space_tree (nums) (max_sum) (index + 1) (Array.append path [|value|]) (curr_sum + value) (remaining_sum - value)
            let mutable j: int = 0
            while j < (Seq.length (subres)) do
                result <- Array.append result [|_idx subres (j)|]
                j <- j + 1
            index <- index + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and generate_sum_of_subsets_solutions (nums: int array) (max_sum: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    let mutable max_sum = max_sum
    try
        let total: int = sum_list (nums)
        __ret <- create_state_space_tree (nums) (max_sum) (0) (Array.empty<int>) (0) (total)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        json (generate_sum_of_subsets_solutions (unbox<int array> [|3; 34; 4; 12; 5; 2|]) (9))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
