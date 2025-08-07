// Generated 2025-08-07 14:57 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable heap: int array = [|0|]
let mutable size: int = 0
let rec swap_up (i: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable i = i
    try
        let temp: int = _idx heap (i)
        let mutable idx: int = i
        while (idx / 2) > 0 do
            if (_idx heap (idx)) > (_idx heap (idx / 2)) then
                heap <- _arrset heap idx (_idx heap (idx / 2))
                heap <- _arrset heap idx / 2 (temp)
            idx <- idx / 2
        __ret
    with
        | Return -> __ret
let rec insert (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable value = value
    try
        heap <- Array.append heap [|value|]
        size <- size + 1
        swap_up (size)
        __ret
    with
        | Return -> __ret
let rec swap_down (i: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable i = i
    try
        let mutable idx: int = i
        while size >= (2 * idx) do
            let bigger_child: int = if ((2 * idx) + 1) > size then (2 * idx) else (if (_idx heap (2 * idx)) > (_idx heap ((2 * idx) + 1)) then (2 * idx) else ((2 * idx) + 1))
            let temp: int = _idx heap (idx)
            if (_idx heap (idx)) < (_idx heap (bigger_child)) then
                heap <- _arrset heap idx (_idx heap (bigger_child))
                heap <- _arrset heap bigger_child (temp)
            idx <- bigger_child
        __ret
    with
        | Return -> __ret
let rec shrink () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable new_heap: int array = [||]
        let mutable i: int = 0
        while i <= size do
            new_heap <- Array.append new_heap [|_idx heap (i)|]
            i <- i + 1
        heap <- new_heap
        __ret
    with
        | Return -> __ret
let rec pop () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let max_value: int = _idx heap (1)
        heap <- _arrset heap 1 (_idx heap (size))
        size <- size - 1
        shrink()
        swap_down (1)
        __ret <- max_value
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_list () =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    try
        let mutable out: int array = [||]
        let mutable i: int = 1
        while i <= size do
            out <- Array.append out [|_idx heap (i)|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec len () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- size
        raise Return
        __ret
    with
        | Return -> __ret
insert (6)
insert (10)
insert (15)
insert (12)
printfn "%d" (pop())
printfn "%d" (pop())
printfn "%s" (_repr (get_list()))
printfn "%d" (len())
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
