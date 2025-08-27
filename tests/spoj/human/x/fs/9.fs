// Generated 2025-08-27 07:05 +0700

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
let rec _str v =
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
open System.Collections.Generic

open System

let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
let rec parseInt (s: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable s = s
    try
        let mutable i: int64 = int64 0
        let mutable n: int64 = int64 0
        while i < (int64 (String.length (s))) do
            n <- (n * (int64 10)) + (int64 (_dictGet digits ((string (_substring s (int i) (int (i + (int64 1))))))))
            i <- i + (int64 1)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable parts: string array = Array.empty<string>
        let mutable cur: string = ""
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            let ch: string = _substring s (int i) (int (i + (int64 1)))
            if ch = " " then
                if (String.length (cur)) > 0 then
                    parts <- Array.append parts [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + (int64 1)
        if (String.length (cur)) > 0 then
            parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
and sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable r: float = x
        let mutable prev: float = 0.0
        while (absf (r - prev)) > 0.000000000001 do
            prev <- r
            r <- (r + (x / r)) / 2.0
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and makeBoolGrid (P: int64) (Q: int64) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable P = P
    let mutable Q = Q
    try
        let mutable g: bool array array = Array.empty<bool array>
        let mutable i: int64 = int64 0
        while i < P do
            let mutable row: bool array = Array.empty<bool>
            let mutable j: int64 = int64 0
            while j < Q do
                row <- Array.append row [|false|]
                j <- j + (int64 1)
            g <- Array.append g [|row|]
            i <- i + (int64 1)
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and visible (grid: int64 array array) (P: int64) (Q: int64) (R: int64) (C: int64) (BR: int64) (BC: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable grid = grid
    let mutable P = P
    let mutable Q = Q
    let mutable R = R
    let mutable C = C
    let mutable BR = BR
    let mutable BC = BC
    try
        let X1: float = (float C) - 0.5
        let Y1: float = (float R) - 0.5
        let Z1: float = (float (_idx (_idx grid (int (int64 (R - (int64 1))))) (int (int64 (C - (int64 1)))))) + 0.5
        let X2: float = (float BC) - 0.5
        let Y2: float = (float BR) - 0.5
        let Z2: float = (float (_idx (_idx grid (int (int64 (BR - (int64 1))))) (int (int64 (BC - (int64 1)))))) + 0.5
        let Dx: float = X2 - X1
        let Dy: float = Y2 - Y1
        let Dz: float = Z2 - Z1
        let dist: float = sqrt (((Dx * Dx) + (Dy * Dy)) + (Dz * Dz))
        let mutable steps: int64 = (int64 (dist * 20.0)) + (int64 1)
        let stepT: float = 1.0 / (float steps)
        let mutable i: int64 = int64 1
        while i < steps do
            let t: float = stepT * (float i)
            let X: float = X1 + (Dx * t)
            let Y: float = Y1 + (Dy * t)
            let Z: float = Z1 + (Dz * t)
            let rIdx: int64 = (int64 Y) + (int64 1)
            let cIdx: int64 = (int64 X) + (int64 1)
            if (((rIdx < (int64 1)) || (rIdx > P)) || (cIdx < (int64 1))) || (cIdx > Q) then
                __ret <- false
                raise Return
            let H: float = float (_idx (_idx grid (int (int64 (rIdx - (int64 1))))) (int (int64 (cIdx - (int64 1)))))
            if Z <= H then
                __ret <- false
                raise Return
            i <- i + (int64 1)
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and computeVis (grid: int64 array array) (P: int64) (Q: int64) (BR: int64) (BC: int64) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable grid = grid
    let mutable P = P
    let mutable Q = Q
    let mutable BR = BR
    let mutable BC = BC
    try
        let mutable vis: bool array array = makeBoolGrid (int64 P) (int64 Q)
        let mutable r: int64 = int64 1
        while r <= P do
            let mutable c: int64 = int64 1
            while c <= Q do
                vis.[int (int64 (r - (int64 1)))].[int (int64 (c - (int64 1)))] <- visible (grid) (int64 P) (int64 Q) (int64 r) (int64 c) (int64 BR) (int64 BC)
                c <- c + (int64 1)
            r <- r + (int64 1)
        __ret <- vis
        raise Return
        __ret
    with
        | Return -> __ret
and bfs (grid: int64 array array) (P: int64) (Q: int64) (R1: int64) (C1: int64) (R2: int64) (C2: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable grid = grid
    let mutable P = P
    let mutable Q = Q
    let mutable R1 = R1
    let mutable C1 = C1
    let mutable R2 = R2
    let mutable C2 = C2
    try
        let vis1: bool array array = computeVis (grid) (int64 P) (int64 Q) (int64 R1) (int64 C1)
        let vis2: bool array array = computeVis (grid) (int64 P) (int64 Q) (int64 R2) (int64 C2)
        let mutable visited: bool array array = makeBoolGrid (int64 P) (int64 Q)
        let mutable qR: int64 array = Array.empty<int64>
        let mutable qC: int64 array = Array.empty<int64>
        let mutable qD: int64 array = Array.empty<int64>
        qR <- Array.append qR [|R1|]
        qC <- Array.append qC [|C1|]
        qD <- Array.append qD [|int64 (0)|]
        visited.[int (int64 (R1 - (int64 1)))].[int (int64 (C1 - (int64 1)))] <- true
        let mutable head: int64 = int64 0
        while head < (int64 (Seq.length (qR))) do
            let mutable r: int64 = _idx qR (int head)
            let mutable c: int64 = _idx qC (int head)
            let d: int64 = _idx qD (int head)
            if (r = R2) && (c = C2) then
                __ret <- d
                raise Return
            let hr: int64 = _idx (_idx grid (int (int64 (r - (int64 1))))) (int (int64 (c - (int64 1))))
            let mutable idx: int64 = int64 0
            while idx < (int64 4) do
                let mutable nr: int64 = r
                let mutable nc: int64 = c
                if idx = (int64 0) then
                    nr <- nr - (int64 1)
                if idx = (int64 1) then
                    nr <- nr + (int64 1)
                if idx = (int64 2) then
                    nc <- nc - (int64 1)
                if idx = (int64 3) then
                    nc <- nc + (int64 1)
                if (((nr >= (int64 1)) && (nr <= P)) && (nc >= (int64 1))) && (nc <= Q) then
                    if not (_idx (_idx visited (int (int64 (nr - (int64 1))))) (int (int64 (nc - (int64 1))))) then
                        let hn: int64 = _idx (_idx grid (int (int64 (nr - (int64 1))))) (int (int64 (nc - (int64 1))))
                        let diff: int64 = hn - hr
                        if (diff <= (int64 1)) && (diff >= (int64 (0 - 3))) then
                            if (_idx (_idx vis1 (int (int64 (nr - (int64 1))))) (int (int64 (nc - (int64 1))))) || (_idx (_idx vis2 (int (int64 (nr - (int64 1))))) (int (int64 (nc - (int64 1))))) then
                                visited.[int (int64 (nr - (int64 1)))].[int (int64 (nc - (int64 1)))] <- true
                                qR <- Array.append qR [|nr|]
                                qC <- Array.append qC [|nc|]
                                qD <- Array.append qD [|(d + (int64 1))|]
                idx <- idx + (int64 1)
            head <- head + (int64 1)
        __ret <- int64 (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tLine: string = _readLine()
        if tLine = "" then
            __ret <- ()
            raise Return
        let t: int64 = parseInt (tLine)
        let mutable case: int64 = int64 0
        while case < t do
            let mutable line: string = _readLine()
            while line = "" do
                line <- _readLine()
            let pq: string array = split (line)
            let P: int64 = parseInt (_idx pq (int 0))
            let Q: int64 = parseInt (_idx pq (int 1))
            let mutable grid: int64 array array = Array.empty<int64 array>
            let mutable r: int64 = int64 0
            while r < P do
                let rowParts: string array = split (_readLine())
                let mutable row: int64 array = Array.empty<int64>
                let mutable c: int64 = int64 0
                while c < Q do
                    row <- Array.append row [|(parseInt (_idx rowParts (int c)))|]
                    c <- c + (int64 1)
                grid <- Array.append grid [|row|]
                r <- r + (int64 1)
            let coords: string array = split (_readLine())
            let R1: int64 = parseInt (_idx coords (int 0))
            let C1: int64 = parseInt (_idx coords (int 1))
            let R2: int64 = parseInt (_idx coords (int 2))
            let C2: int64 = parseInt (_idx coords (int 3))
            let res: int64 = bfs (grid) (int64 P) (int64 Q) (int64 R1) (int64 C1) (int64 R2) (int64 C2)
            if res < (int64 0) then
                ignore (printfn "%s" ("Mission impossible!"))
            else
                ignore (printfn "%s" (("The shortest path is " + (_str (res))) + " steps long."))
            case <- case + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
