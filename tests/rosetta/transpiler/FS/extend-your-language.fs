// Generated 2025-08-04 20:03 +0700

exception Return
let mutable __ret = ()

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
type If2 = {
    cond1: bool
    cond2: bool
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec else1 (i: If2) (f: unit -> unit) =
    let mutable __ret : If2 = Unchecked.defaultof<If2>
    let mutable i = i
    let mutable f = f
    try
        if (i.cond1) && ((i.cond2) = false) then
            f()
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and else2 (i: If2) (f: unit -> unit) =
    let mutable __ret : If2 = Unchecked.defaultof<If2>
    let mutable i = i
    let mutable f = f
    try
        if (i.cond2) && ((i.cond1) = false) then
            f()
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and else0 (i: If2) (f: unit -> unit) =
    let mutable __ret : If2 = Unchecked.defaultof<If2>
    let mutable i = i
    let mutable f = f
    try
        if ((i.cond1) = false) && ((i.cond2) = false) then
            f()
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and if2 (cond1: bool) (cond2: bool) (f: unit -> unit) =
    let mutable __ret : If2 = Unchecked.defaultof<If2>
    let mutable cond1 = cond1
    let mutable cond2 = cond2
    let mutable f = f
    try
        if cond1 && cond2 then
            f()
        __ret <- { cond1 = cond1; cond2 = cond2 }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int = 0
let mutable b: int = 1
let mutable t: If2 = if2 (a = 1) (b = 3) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a = 1 and b = 3")
        __ret
    with
        | Return -> __ret))
t <- else1 (t) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a = 1 and b <> 3")
        __ret
    with
        | Return -> __ret))
t <- else2 (t) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a <> 1 and b = 3")
        __ret
    with
        | Return -> __ret))
else0 (t) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a <> 1 and b <> 3")
        __ret
    with
        | Return -> __ret))
a <- 1
b <- 0
t <- if2 (a = 1) (b = 3) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a = 1 and b = 3")
        __ret
    with
        | Return -> __ret))
t <- else0 (t) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a <> 1 and b <> 3")
        __ret
    with
        | Return -> __ret))
else1 (t) (unbox<unit -> unit> (fun () -> 
    let mutable __ret = ()
    try
        printfn "%s" ("a = 1 and b <> 3")
        __ret
    with
        | Return -> __ret))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
