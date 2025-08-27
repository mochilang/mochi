// Generated 2025-08-26 14:25 +0700

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

let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
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
type Edge = {
    mutable ``to``: int64
    mutable _cost: int64
}
open System.Collections.Generic

open System

let rec parseIntStr (str: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable str = str
    try
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        let mutable i: int64 = int64 0
        let mutable n: int64 = int64 0
        while i < (int64 (String.length (str))) do
            n <- (n * (int64 10)) + (int64 (_dictGet digits ((string (_substring str (int i) (int (i + (int64 1))))))))
            i <- i + (int64 1)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and split_two (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            let ch: string = _substring s (int i) (int (i + (int64 1)))
            if ch = " " then
                if current <> "" then
                    res <- Array.append res [|current|]
                    current <- ""
            else
                current <- current + ch
            i <- i + (int64 1)
        if current <> "" then
            res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and make_int_list (n: int64) (value: int64) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable n = n
    let mutable value = value
    try
        let mutable lst: int64 array = Array.empty<int64>
        let mutable i: int64 = int64 0
        while i < n do
            lst <- Array.append lst [|value|]
            i <- i + (int64 1)
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and make_bool_list (n: int64) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    try
        let mutable lst: bool array = Array.empty<bool>
        let mutable i: int64 = int64 0
        while i < n do
            lst <- Array.append lst [|false|]
            i <- i + (int64 1)
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and make_edge_list (n: int64) =
    let mutable __ret : Edge array array = Unchecked.defaultof<Edge array array>
    let mutable n = n
    try
        let mutable lst: Edge array array = Array.empty<Edge array>
        let mutable i: int64 = int64 0
        while i <= n do
            lst <- Array.append lst [|Array.empty<Edge>|]
            i <- i + (int64 1)
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and dijkstra (graph: Edge array array) (start: int64) (dest: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable graph = graph
    let mutable start = start
    let mutable dest = dest
    try
        let mutable n: int64 = int64 (Seq.length (graph))
        let inf: int64 = int64 1000000000
        let mutable dist: int64 array = make_int_list (int64 n) (int64 inf)
        let mutable visited: bool array = make_bool_list (int64 n)
        dist.[int start] <- int64 0
        let mutable count: int64 = int64 0
        try
            while count < n do
                try
                    let mutable u: int64 = int64 0
                    let mutable best: int64 = inf
                    let mutable i: int64 = int64 1
                    while i < n do
                        if (not (_idx visited (int i))) && ((_idx dist (int i)) < best) then
                            best <- _idx dist (int i)
                            u <- i
                        i <- i + (int64 1)
                    if (u = (int64 0)) || (u = dest) then
                        raise Break
                    visited.[int u] <- true
                    let mutable j: int64 = int64 0
                    while j < (int64 (Seq.length (_idx graph (int u)))) do
                        let e: Edge = _idx (_idx graph (int u)) (int j)
                        let v: int64 = e.``to``
                        let w: int64 = e._cost
                        if not (_idx visited (int v)) then
                            let alt: int64 = (_idx dist (int u)) + w
                            if alt < (_idx dist (int v)) then
                                dist.[int v] <- alt
                        j <- j + (int64 1)
                    count <- count + (int64 1)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- _idx dist (int dest)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tStr: string = _readLine()
        if tStr = "" then
            __ret <- ()
            raise Return
        let t: int64 = parseIntStr (tStr)
        let mutable caseIndex: int64 = int64 0
        while caseIndex < t do
            let nStr: string = _readLine()
            let mutable n: int64 = parseIntStr (nStr)
            let mutable graph: Edge array array = make_edge_list (int64 n)
            let mutable nameToIdx: System.Collections.Generic.IDictionary<string, int64> = _dictCreate []
            let mutable i: int64 = int64 1
            while i <= n do
                let city: string = _readLine()
                nameToIdx <- _dictAdd (nameToIdx) (string (city)) (i)
                let pStr: string = _readLine()
                let p: int64 = parseIntStr (pStr)
                let mutable j: int64 = int64 0
                while j < p do
                    let line: string = _readLine()
                    let parts: string array = split_two (line)
                    let nr: int64 = parseIntStr (_idx parts (int 0))
                    let _cost: int64 = parseIntStr (_idx parts (int 1))
                    graph.[int i] <- Array.append (_idx graph (int i)) [|{ ``to`` = nr; _cost = _cost }|]
                    j <- j + (int64 1)
                i <- i + (int64 1)
            let rStr: string = _readLine()
            let r: int64 = parseIntStr (rStr)
            let mutable k: int64 = int64 0
            while k < r do
                let line: string = _readLine()
                let parts: string array = split_two (line)
                let src: int64 = _dictGet nameToIdx ((string (_idx parts (int 0))))
                let dst: int64 = _dictGet nameToIdx ((string (_idx parts (int 1))))
                let mutable res: int64 = dijkstra (graph) (int64 src) (int64 dst)
                ignore (printfn "%d" (res))
                k <- k + (int64 1)
            if caseIndex < (t - (int64 1)) then
                ignore (_readLine())
            caseIndex <- caseIndex + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
