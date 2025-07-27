// Generated 2025-07-27 23:36 +0700

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

type Point = {
    x: float
    y: float
    z: float
}
type Edge = {
    pn1: int
    pn2: int
    fn1: int
    fn2: int
    cp: Point
}
type PointEx = {
    p: Point
    n: int
}
let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and fmt4 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = x * 10000.0
        if y >= (float 0) then
            y <- y + 0.5
        else
            y <- y - 0.5
        y <- (float (int y)) / 10000.0
        let mutable s: string = string y
        let mutable dot: int = indexOf s "."
        if dot = (0 - 1) then
            s <- s + ".0000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            if decs > 4 then
                s <- _substring s 0 (dot + 5)
            else
                while decs < 4 do
                    s <- s + "0"
                    decs <- decs + 1
        if x >= 0.0 then
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and fmt2 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let s: string = string n
        if (String.length s) < 2 then
            __ret <- " " + s
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and sumPoint (p1: Point) (p2: Point) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        __ret <- { x = (p1.x) + (p2.x); y = (p1.y) + (p2.y); z = (p1.z) + (p2.z) }
        raise Return
        __ret
    with
        | Return -> __ret
and mulPoint (p: Point) (m: float) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable p = p
    let mutable m = m
    try
        __ret <- { x = (p.x) * m; y = (p.y) * m; z = (p.z) * m }
        raise Return
        __ret
    with
        | Return -> __ret
and divPoint (p: Point) (d: float) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable p = p
    let mutable d = d
    try
        __ret <- mulPoint p (1.0 / d)
        raise Return
        __ret
    with
        | Return -> __ret
and centerPoint (p1: Point) (p2: Point) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        __ret <- divPoint (sumPoint p1 p2) 2.0
        raise Return
        __ret
    with
        | Return -> __ret
and getFacePoints (points: Point array) (faces: int array array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable points = points
    let mutable faces = faces
    try
        let mutable facePoints: Point array = [||]
        let mutable i: int = 0
        while i < (int (Array.length faces)) do
            let face: int array = faces.[i]
            let mutable fp: Point = { x = 0.0; y = 0.0; z = 0.0 }
            for idx in face do
                fp <- sumPoint fp (unbox<Point> (points.[idx]))
            fp <- divPoint fp (float (Array.length face))
            facePoints <- unbox<Point array> (Array.append facePoints [|fp|])
            i <- i + 1
        __ret <- unbox<Point array> facePoints
        raise Return
        __ret
    with
        | Return -> __ret
and sortEdges (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable edges = edges
    try
        let mutable res: int array array = [||]
        let mutable tmp: int array array = edges
        while (int (Array.length tmp)) > 0 do
            let mutable min: int array = tmp.[0]
            let mutable idx: int = 0
            let mutable j: int = 1
            while j < (int (Array.length tmp)) do
                let e: int array = tmp.[j]
                if ((e.[0]) < (min.[0])) || (((e.[0]) = (min.[0])) && (((e.[1]) < (min.[1])) || (((e.[1]) = (min.[1])) && ((e.[2]) < (min.[2]))))) then
                    min <- unbox<int array> e
                    idx <- j
                j <- j + 1
            res <- unbox<int array array> (Array.append res [|min|])
            let mutable out: int array array = [||]
            let mutable k: int = 0
            while k < (int (Array.length tmp)) do
                if k <> idx then
                    out <- unbox<int array array> (Array.append out [|tmp.[k]|])
                k <- k + 1
            tmp <- unbox<int array array> out
        __ret <- unbox<int array array> res
        raise Return
        __ret
    with
        | Return -> __ret
and getEdgesFaces (points: Point array) (faces: int array array) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable points = points
    let mutable faces = faces
    try
        let mutable edges: int array array = [||]
        let mutable fnum: int = 0
        while fnum < (int (Array.length faces)) do
            let face: int array = faces.[fnum]
            let mutable numP: int = Array.length face
            let mutable pi: int = 0
            while pi < numP do
                let mutable pn1: int = face.[pi]
                let mutable pn2: int = 0
                if pi < (numP - 1) then
                    pn2 <- int (face.[pi + 1])
                else
                    pn2 <- int (face.[0])
                if pn1 > pn2 then
                    let mutable tmpn: int = pn1
                    pn1 <- pn2
                    pn2 <- tmpn
                edges <- unbox<int array array> (Array.append edges [|[|pn1; pn2; fnum|]|])
                pi <- pi + 1
            fnum <- fnum + 1
        edges <- sortEdges edges
        let mutable merged: int array array = [||]
        let mutable idx: int = 0
        try
            while idx < (int (Array.length edges)) do
                try
                    let e1: int array = edges.[idx]
                    if idx < (int ((int (Array.length edges)) - 1)) then
                        let e2: int array = edges.[idx + 1]
                        if ((e1.[0]) = (e2.[0])) && ((e1.[1]) = (e2.[1])) then
                            merged <- unbox<int array array> (Array.append merged [|[|e1.[0]; e1.[1]; e1.[2]; e2.[2]|]|])
                            idx <- idx + 2
                            raise Continue
                    merged <- unbox<int array array> (Array.append merged [|[|e1.[0]; e1.[1]; e1.[2]; -1|]|])
                    idx <- idx + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable edgesCenters: Edge array = [||]
        for me in merged do
            let p1: Point = points.[me.[0]]
            let p2: Point = points.[me.[1]]
            let cp: Point = centerPoint p1 p2
            edgesCenters <- unbox<Edge array> (Array.append edgesCenters [|{ pn1 = me.[0]; pn2 = me.[1]; fn1 = me.[2]; fn2 = me.[3]; cp = cp }|])
        __ret <- unbox<Edge array> edgesCenters
        raise Return
        __ret
    with
        | Return -> __ret
and getEdgePoints (points: Point array) (edgesFaces: Edge array) (facePoints: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable points = points
    let mutable edgesFaces = edgesFaces
    let mutable facePoints = facePoints
    try
        let mutable edgePoints: Point array = [||]
        let mutable i: int = 0
        while i < (int (Array.length edgesFaces)) do
            let edge: Edge = edgesFaces.[i]
            let cp: Point = edge.cp
            let fp1: Point = facePoints.[edge.fn1]
            let mutable fp2: Point = fp1
            if (edge.fn2) <> (0 - 1) then
                fp2 <- unbox<Point> (facePoints.[edge.fn2])
            let cfp: Point = centerPoint fp1 fp2
            edgePoints <- unbox<Point array> (Array.append edgePoints [|centerPoint cp cfp|])
            i <- i + 1
        __ret <- unbox<Point array> edgePoints
        raise Return
        __ret
    with
        | Return -> __ret
and getAvgFacePoints (points: Point array) (faces: int array array) (facePoints: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable points = points
    let mutable faces = faces
    let mutable facePoints = facePoints
    try
        let mutable numP: int = Array.length points
        let mutable temp: PointEx array = [||]
        let mutable i: int = 0
        while i < numP do
            temp <- unbox<PointEx array> (Array.append temp [|{ p = { x = 0.0; y = 0.0; z = 0.0 }; n = 0 }|])
            i <- i + 1
        let mutable fnum: int = 0
        while fnum < (int (Array.length faces)) do
            let fp: Point = facePoints.[fnum]
            for pn in faces.[fnum] do
                let tp: PointEx = temp.[pn]
                temp.[pn] <- { p = sumPoint (tp.p) fp; n = (tp.n) + 1 }
            fnum <- fnum + 1
        let mutable avg: Point array = [||]
        let mutable j: int = 0
        while j < numP do
            let tp: PointEx = temp.[j]
            avg <- unbox<Point array> (Array.append avg [|divPoint (tp.p) (float (tp.n))|])
            j <- j + 1
        __ret <- unbox<Point array> avg
        raise Return
        __ret
    with
        | Return -> __ret
and getAvgMidEdges (points: Point array) (edgesFaces: Edge array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable points = points
    let mutable edgesFaces = edgesFaces
    try
        let mutable numP: int = Array.length points
        let mutable temp: PointEx array = [||]
        let mutable i: int = 0
        while i < numP do
            temp <- unbox<PointEx array> (Array.append temp [|{ p = { x = 0.0; y = 0.0; z = 0.0 }; n = 0 }|])
            i <- i + 1
        for edge in edgesFaces do
            let cp: Point = edge.cp
            let mutable arr: int array = [|edge.pn1; edge.pn2|]
            for pn in arr do
                let tp: PointEx = temp.[pn]
                temp.[pn] <- { p = sumPoint (tp.p) cp; n = (tp.n) + 1 }
        let mutable avg: Point array = [||]
        let mutable j: int = 0
        while j < numP do
            let tp: PointEx = temp.[j]
            avg <- unbox<Point array> (Array.append avg [|divPoint (tp.p) (float (tp.n))|])
            j <- j + 1
        __ret <- unbox<Point array> avg
        raise Return
        __ret
    with
        | Return -> __ret
and getPointsFaces (points: Point array) (faces: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable points = points
    let mutable faces = faces
    try
        let mutable pf: int array = [||]
        let mutable i: int = 0
        while i < (int (Array.length points)) do
            pf <- unbox<int array> (Array.append pf [|0|])
            i <- i + 1
        let mutable fnum: int = 0
        while fnum < (int (Array.length faces)) do
            for pn in faces.[fnum] do
                pf.[pn] <- (int (pf.[pn])) + 1
            fnum <- fnum + 1
        __ret <- unbox<int array> pf
        raise Return
        __ret
    with
        | Return -> __ret
and getNewPoints (points: Point array) (pf: int array) (afp: Point array) (ame: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable points = points
    let mutable pf = pf
    let mutable afp = afp
    let mutable ame = ame
    try
        let mutable newPts: Point array = [||]
        let mutable i: int = 0
        while i < (int (Array.length points)) do
            let mutable n: float = float (pf.[i])
            let mutable m1: float = (n - 3.0) / n
            let mutable m2: float = 1.0 / n
            let mutable m3: float = 2.0 / n
            let old: Point = points.[i]
            let p1: Point = mulPoint old m1
            let p2: Point = mulPoint (unbox<Point> (afp.[i])) m2
            let p3: Point = mulPoint (unbox<Point> (ame.[i])) m3
            newPts <- unbox<Point array> (Array.append newPts [|sumPoint (sumPoint p1 p2) p3|])
            i <- i + 1
        __ret <- unbox<Point array> newPts
        raise Return
        __ret
    with
        | Return -> __ret
and key (a: int) (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then (((string a) + ",") + (string b)) else (((string b) + ",") + (string a))
        raise Return
        __ret
    with
        | Return -> __ret
and cmcSubdiv (points: Point array) (faces: int array array) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable points = points
    let mutable faces = faces
    try
        let facePoints: Point array = getFacePoints points faces
        let edgesFaces: Edge array = getEdgesFaces points faces
        let edgePoints: Point array = getEdgePoints points edgesFaces facePoints
        let avgFacePoints: Point array = getAvgFacePoints points faces facePoints
        let avgMidEdges: Point array = getAvgMidEdges points edgesFaces
        let pointsFaces: int array = getPointsFaces points faces
        let mutable newPoints: Point array = getNewPoints points pointsFaces avgFacePoints avgMidEdges
        let mutable facePointNums: int array = [||]
        let mutable nextPoint: int = Array.length newPoints
        for fp in facePoints do
            newPoints <- unbox<Point array> (Array.append newPoints [|fp|])
            facePointNums <- unbox<int array> (Array.append facePointNums [|nextPoint|])
            nextPoint <- nextPoint + 1
        let mutable edgePointNums: Map<string, int> = Map.ofList []
        let mutable idx: int = 0
        while idx < (int (Array.length edgesFaces)) do
            let e: Edge = edgesFaces.[idx]
            newPoints <- unbox<Point array> (Array.append newPoints [|edgePoints.[idx]|])
            edgePointNums <- Map.add (key (e.pn1) (e.pn2)) nextPoint edgePointNums
            nextPoint <- nextPoint + 1
            idx <- idx + 1
        let mutable newFaces: int array array = [||]
        let mutable fnum: int = 0
        while fnum < (int (Array.length faces)) do
            let oldFace: int array = faces.[fnum]
            if (int (Array.length oldFace)) = 4 then
                let a: int = oldFace.[0]
                let b: int = oldFace.[1]
                let c: int = oldFace.[2]
                let d: int = oldFace.[3]
                let fpnum: int = facePointNums.[fnum]
                let ab: int = edgePointNums.[(key a b)] |> unbox<int>
                let da: int = edgePointNums.[(key d a)] |> unbox<int>
                let bc: int = edgePointNums.[(key b c)] |> unbox<int>
                let cd: int = edgePointNums.[(key c d)] |> unbox<int>
                newFaces <- unbox<int array array> (Array.append newFaces [|[|a; ab; fpnum; da|]|])
                newFaces <- unbox<int array array> (Array.append newFaces [|[|b; bc; fpnum; ab|]|])
                newFaces <- unbox<int array array> (Array.append newFaces [|[|c; cd; fpnum; bc|]|])
                newFaces <- unbox<int array array> (Array.append newFaces [|[|d; da; fpnum; cd|]|])
            fnum <- fnum + 1
        __ret <- [|box newPoints; box newFaces|]
        raise Return
        __ret
    with
        | Return -> __ret
and formatPoint (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((((("[" + (unbox<string> (fmt4 (p.x)))) + " ") + (unbox<string> (fmt4 (p.y)))) + " ") + (unbox<string> (fmt4 (p.z)))) + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and formatFace (f: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        if (int (Array.length f)) = 0 then
            __ret <- "[]"
            raise Return
        let mutable s: string = "[" + (unbox<string> (fmt2 (int (f.[0]))))
        let mutable i: int = 1
        while i < (int (Array.length f)) do
            s <- (s + " ") + (unbox<string> (fmt2 (int (f.[i]))))
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let inputPoints: Point array = [|{ x = -1.0; y = 1.0; z = 1.0 }; { x = -1.0; y = -1.0; z = 1.0 }; { x = 1.0; y = -1.0; z = 1.0 }; { x = 1.0; y = 1.0; z = 1.0 }; { x = 1.0; y = -1.0; z = -1.0 }; { x = 1.0; y = 1.0; z = -1.0 }; { x = -1.0; y = -1.0; z = -1.0 }; { x = -1.0; y = 1.0; z = -1.0 }|]
        let inputFaces: int array array = [|[|0; 1; 2; 3|]; [|3; 2; 4; 5|]; [|5; 4; 6; 7|]; [|7; 0; 3; 5|]; [|7; 6; 1; 0|]; [|6; 1; 2; 4|]|]
        let mutable outputPoints: Point array = inputPoints
        let mutable outputFaces: int array array = inputFaces
        let mutable i: int = 0
        while i < 1 do
            let res: obj array = cmcSubdiv outputPoints outputFaces
            outputPoints <- unbox<Point array> (res.[0])
            outputFaces <- unbox<int array array> (res.[1])
            i <- i + 1
        for p in outputPoints do
            printfn "%s" (formatPoint p)
        printfn "%s" ""
        for f in outputFaces do
            printfn "%s" (formatFace f)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
