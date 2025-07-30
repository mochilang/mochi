// Generated 2025-07-30 21:05 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable i: int = (((String.length s) % 3 + 3) % 3)
        if i = 0 then
            i <- 3
        let mutable out: string = _substring s 0 i
        while i < (String.length s) do
            out <- (out + ",") + (_substring s i (i + 3))
            i <- i + 3
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec p (L: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable L = L
    let mutable n = n
    try
        let Ls: string = string L
        let mutable digits: int = 1
        let mutable d: int = 1
        while d <= (18 - (String.length Ls)) do
            digits <- digits * 10
            d <- d + 1
        let ten18: int = 999999995705032704
        let mutable count: int = 0
        let mutable i: int = 0
        let mutable probe: int = 1
        try
            while true do
                try
                    probe <- probe * 2
                    i <- i + 1
                    if (int64 probe) >= ten18 then
                        try
                            while true do
                                try
                                    if (int64 probe) >= ten18 then
                                        probe <- probe / 10
                                    if (probe / digits) = L then
                                        count <- count + 1
                                        if count >= n then
                                            count <- count - 1
                                            raise Break
                                    probe <- probe * 2
                                    i <- i + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                    let mutable ps: string = string probe
                    let mutable le: int = String.length Ls
                    if le > (String.length ps) then
                        le <- String.length ps
                    if (_substring ps 0 le) = Ls then
                        count <- count + 1
                        if count >= n then
                            raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
let mutable params: int array array = [|[|12; 1|]; [|12; 2|]; [|123; 45|]|]
let mutable idx: int = 0
while idx < (Seq.length params) do
    let L: int = (params.[idx]).[0]
    let m: int = (params.[idx]).[1]
    printfn "%s" ((((("p(" + (string L)) + ", ") + (string m)) + ") = ") + (unbox<string> (commatize (p L m))))
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
