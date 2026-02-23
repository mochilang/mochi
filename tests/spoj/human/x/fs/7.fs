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
open System

open System.Collections.Generic

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = Array.empty<string>
        let mutable cur: string = ""
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            if (((String.length (sep)) > 0) && ((i + (int64 (String.length (sep)))) <= (int64 (String.length (s))))) && ((_substring s (int i) (int (i + (int64 (String.length (sep)))))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (int64 (String.length (sep)))
            else
                cur <- cur + (_substring s (int i) (int (i + (int64 1))))
                i <- i + (int64 1)
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and parse_ints (line: string) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable line = line
    try
        let pieces: string array = split (line) (" ")
        let mutable nums: int64 array = Array.empty<int64>
        let mutable i: int64 = int64 0
        while i < (int64 (Seq.length (pieces))) do
            let p: string = _idx pieces (int i)
            if (String.length (p)) > 0 then
                nums <- Array.append nums [|(int64 p)|]
            i <- i + (int64 1)
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
and sort_unique (arr: int64 array) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable arr = arr
    try
        let mutable i: int64 = int64 1
        while i < (int64 (Seq.length (arr))) do
            let mutable j: int64 = i
            while (j > (int64 0)) && ((_idx arr (int (j - (int64 1)))) > (_idx arr (int j))) do
                let tmp: int64 = _idx arr (int (j - (int64 1)))
                arr.[int (j - (int64 1))] <- _idx arr (int j)
                arr.[int j] <- tmp
                j <- j - (int64 1)
            i <- i + (int64 1)
        let mutable res: int64 array = Array.empty<int64>
        i <- int64 0
        while i < (int64 (Seq.length (arr))) do
            if (i = (int64 0)) || ((_idx arr (int i)) <> (_idx arr (int (i - (int64 1))))) then
                res <- Array.append res [|(_idx arr (int i))|]
            i <- i + (int64 1)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and pointInPoly (xs: int64 array) (ys: int64 array) (px: float) (py: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable ys = ys
    let mutable px = px
    let mutable py = py
    try
        let mutable inside: bool = false
        let mutable i: int64 = int64 0
        let mutable j: int64 = int64 ((Seq.length (xs)) - 1)
        while i < (int64 (Seq.length (xs))) do
            let xi: float = float (_idx xs (int i))
            let yi: float = float (_idx ys (int i))
            let xj: float = float (_idx xs (int j))
            let yj: float = float (_idx ys (int j))
            if ((yi > py) && (yj <= py)) || ((yj > py) && (yi <= py)) then
                let xint: float = (((xj - xi) * (py - yi)) / (yj - yi)) + xi
                if px < xint then
                    inside <- not inside
            j <- i
            i <- i + (int64 1)
        __ret <- inside
        raise Return
        __ret
    with
        | Return -> __ret
and make3DBool (a: int64) (b: int64) (c: int64) =
    let mutable __ret : bool array array array = Unchecked.defaultof<bool array array array>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable arr: bool array array array = Array.empty<bool array array>
        let mutable i: int64 = int64 0
        while i < a do
            let mutable plane: bool array array = Array.empty<bool array>
            let mutable j: int64 = int64 0
            while j < b do
                let mutable row: bool array = Array.empty<bool>
                let mutable k: int64 = int64 0
                while k < c do
                    row <- Array.append row [|false|]
                    k <- k + (int64 1)
                plane <- Array.append plane [|row|]
                j <- j + (int64 1)
            arr <- Array.append arr [|plane|]
            i <- i + (int64 1)
        __ret <- arr
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
        let t: int64 = int64 tLine
        let mutable case: int64 = int64 0
        while case < t do
            let fLine: string = _readLine()
            let F: int64 = int64 fLine
            let mutable xs: int64 array = Array.empty<int64>
            let mutable ys: int64 array = Array.empty<int64>
            let mutable zs: int64 array = Array.empty<int64>
            xs <- Array.append xs [|int64 (0)|]
            xs <- Array.append xs [|int64 (1001)|]
            ys <- Array.append ys [|int64 (0)|]
            ys <- Array.append ys [|int64 (1001)|]
            zs <- Array.append zs [|int64 (0)|]
            zs <- Array.append zs [|int64 (1001)|]
            let mutable faceXCoord: int64 array = Array.empty<int64>
            let mutable faceYPoly: int64 array array = Array.empty<int64 array>
            let mutable faceZPoly: int64 array array = Array.empty<int64 array>
            let mutable i: int64 = int64 0
            while i < F do
                let line: string = _readLine()
                let mutable nums: int64 array = parse_ints (line)
                let P: int64 = _idx nums (int 0)
                let mutable ptsX: int64 array = Array.empty<int64>
                let mutable ptsY: int64 array = Array.empty<int64>
                let mutable ptsZ: int64 array = Array.empty<int64>
                let mutable j: int64 = int64 0
                while j < P do
                    let x: int64 = _idx nums (int ((int64 1) + ((int64 3) * j)))
                    let y: int64 = _idx nums (int (((int64 1) + ((int64 3) * j)) + (int64 1)))
                    let z: int64 = _idx nums (int (((int64 1) + ((int64 3) * j)) + (int64 2)))
                    ptsX <- Array.append ptsX [|x|]
                    ptsY <- Array.append ptsY [|y|]
                    ptsZ <- Array.append ptsZ [|z|]
                    xs <- Array.append xs [|x|]
                    ys <- Array.append ys [|y|]
                    zs <- Array.append zs [|z|]
                    j <- j + (int64 1)
                let mutable allSame: bool = true
                j <- int64 1
                while j < P do
                    if (_idx ptsX (int j)) <> (_idx ptsX (int 0)) then
                        allSame <- false
                    j <- j + (int64 1)
                if allSame then
                    faceXCoord <- Array.append faceXCoord [|(_idx ptsX (int 0))|]
                    faceYPoly <- Array.append faceYPoly [|ptsY|]
                    faceZPoly <- Array.append faceZPoly [|ptsZ|]
                i <- i + (int64 1)
            xs <- sort_unique (xs)
            ys <- sort_unique (ys)
            zs <- sort_unique (zs)
            let nx: int64 = int64 ((Seq.length (xs)) - 1)
            let ny: int64 = int64 ((Seq.length (ys)) - 1)
            let nz: int64 = int64 ((Seq.length (zs)) - 1)
            let mutable xIndex: System.Collections.Generic.IDictionary<int64, int64> = _dictCreate []
            i <- int64 0
            while i < (int64 (Seq.length (xs))) do
                xIndex <- _dictAdd (xIndex) (_idx xs (int i)) (i)
                i <- i + (int64 1)
            let mutable dx: int64 array = Array.empty<int64>
            i <- int64 0
            while i < nx do
                dx <- Array.append dx [|((_idx xs (int (i + (int64 1)))) - (_idx xs (int i)))|]
                i <- i + (int64 1)
            let mutable dy: int64 array = Array.empty<int64>
            i <- int64 0
            while i < ny do
                dy <- Array.append dy [|((_idx ys (int (i + (int64 1)))) - (_idx ys (int i)))|]
                i <- i + (int64 1)
            let mutable dz: int64 array = Array.empty<int64>
            i <- int64 0
            while i < nz do
                dz <- Array.append dz [|((_idx zs (int (i + (int64 1)))) - (_idx zs (int i)))|]
                i <- i + (int64 1)
            let mutable blockX: bool array array array = make3DBool (int64 (Seq.length (xs))) (int64 ny) (int64 nz)
            i <- int64 0
            while i < (int64 (Seq.length (faceXCoord))) do
                let coord: int64 = _idx faceXCoord (int i)
                let polyY: int64 array = _idx faceYPoly (int i)
                let polyZ: int64 array = _idx faceZPoly (int i)
                let xi: int64 = _dictGet xIndex (coord)
                let mutable j: int64 = int64 0
                while j < ny do
                    let cy: float = (float ((_idx ys (int j)) + (_idx ys (int (j + (int64 1)))))) / 2.0
                    let mutable k: int64 = int64 0
                    while k < nz do
                        let cz: float = (float ((_idx zs (int k)) + (_idx zs (int (k + (int64 1)))))) / 2.0
                        if pointInPoly (polyY) (polyZ) (cy) (cz) then
                            blockX.[int xi].[int j].[int k] <- true
                        k <- k + (int64 1)
                    j <- j + (int64 1)
                i <- i + (int64 1)
            let mutable solid: bool array array array = make3DBool (int64 nx) (int64 ny) (int64 nz)
            let mutable j2: int64 = int64 0
            while j2 < ny do
                let mutable k2: int64 = int64 0
                while k2 < nz do
                    let mutable inside: bool = false
                    let mutable i2: int64 = int64 0
                    while i2 < nx do
                        if _idx (_idx (_idx blockX (int i2)) (int j2)) (int k2) then
                            inside <- not inside
                        if inside then
                            solid.[int i2].[int j2].[int k2] <- true
                        i2 <- i2 + (int64 1)
                    k2 <- k2 + (int64 1)
                j2 <- j2 + (int64 1)
            let mutable volume: int64 = int64 0
            let mutable i3: int64 = int64 0
            while i3 < nx do
                let mutable j3: int64 = int64 0
                while j3 < ny do
                    let mutable k3: int64 = int64 0
                    while k3 < nz do
                        if _idx (_idx (_idx solid (int i3)) (int j3)) (int k3) then
                            volume <- volume + (((_idx dx (int i3)) * (_idx dy (int j3))) * (_idx dz (int k3)))
                        k3 <- k3 + (int64 1)
                    j3 <- j3 + (int64 1)
                i3 <- i3 + (int64 1)
            ignore (printfn "%s" (("The bulk is composed of " + (_str (volume))) + " units."))
            case <- case + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
