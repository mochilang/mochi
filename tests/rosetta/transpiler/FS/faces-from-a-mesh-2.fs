// Generated 2025-08-04 20:23 +0700

exception Break
exception Continue

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type Edge = {
    a: int
    b: int
}
let rec contains (xs: int array) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable v = v
    try
        for x in xs do
            if x = v then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and copyInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable out: int array = [||]
        for x in xs do
            out <- Array.append out [|x|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and sliceEqual (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (i)) <> (_idx b (i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and reverse (xs: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable xs = xs
    try
        let mutable i: int = 0
        let mutable j: int = (Seq.length (xs)) - 1
        while i < j do
            let t: int = _idx xs (i)
            xs.[i] <- _idx xs (j)
            xs.[j] <- t
            i <- i + 1
            j <- j - 1
        __ret
    with
        | Return -> __ret
and perimEqual (p1: int array) (p2: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        if (Seq.length (p1)) <> (Seq.length (p2)) then
            __ret <- false
            raise Return
        for v in p1 do
            if not (contains (p2) (v)) then
                __ret <- false
                raise Return
        let mutable c: int array = copyInts (p1)
        let mutable r: int = 0
        while r < 2 do
            let mutable i: int = 0
            while i < (Seq.length (c)) do
                if sliceEqual (c) (p2) then
                    __ret <- true
                    raise Return
                let t: int = _idx c ((Seq.length (c)) - 1)
                let mutable j: int = (Seq.length (c)) - 1
                while j > 0 do
                    c.[j] <- _idx c (j - 1)
                    j <- j - 1
                c.[0] <- t
                i <- i + 1
            reverse (c)
            r <- r + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and sortEdges (es: Edge array) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable es = es
    try
        let mutable arr: Edge array = es
        let mutable n: int = Seq.length (arr)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                let a: Edge = _idx arr (j)
                let b: Edge = _idx arr (j + 1)
                if ((a.a) > (b.a)) || (((a.a) = (b.a)) && ((a.b) > (b.b))) then
                    arr.[j] <- b
                    arr.[j + 1] <- a
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and concat (a: Edge array) (b: Edge array) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: Edge array = [||]
        for x in a do
            out <- Array.append out [|x|]
        for x in b do
            out <- Array.append out [|x|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and faceToPerim (face: Edge array) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable face = face
    try
        let mutable le: int = Seq.length (face)
        if le = 0 then
            __ret <- null
            raise Return
        let mutable edges: Edge array = [||]
        let mutable i: int = 0
        while i < le do
            let e: Edge = _idx face (i)
            if (e.b) <= (e.a) then
                __ret <- null
                raise Return
            edges <- Array.append edges [|e|]
            i <- i + 1
        edges <- sortEdges (edges)
        let mutable firstEdge: Edge = _idx edges (0)
        let mutable perim: int array = [|firstEdge.a; firstEdge.b|]
        let mutable first: int = firstEdge.a
        let mutable last: int = firstEdge.b
        edges <- Array.sub edges 1 ((Seq.length (edges)) - 1)
        le <- Seq.length (edges)
        let mutable ``done``: bool = false
        try
            while (le > 0) && (not ``done``) do
                try
                    let mutable idx: int = 0
                    let mutable found: bool = false
                    try
                        while idx < le do
                            try
                                let e: Edge = _idx edges (idx)
                                if (e.a) = last then
                                    perim <- Array.append perim [|e.b|]
                                    last <- e.b
                                    found <- true
                                else
                                    if (e.b) = last then
                                        perim <- Array.append perim [|e.a|]
                                        last <- e.a
                                        found <- true
                                if found then
                                    edges <- concat (Array.sub edges 0 (idx - 0)) (Array.sub edges (idx + 1) ((Seq.length (edges)) - (idx + 1)))
                                    le <- le - 1
                                    if last = first then
                                        if le = 0 then
                                            ``done`` <- true
                                        else
                                            __ret <- null
                                            raise Return
                                    raise Break
                                idx <- idx + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        __ret <- null
                        raise Return
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- (Array.sub perim 0 (((Seq.length (perim)) - 1) - 0))
        raise Return
        __ret
    with
        | Return -> __ret
and listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (string (_idx xs (i)))
            if i < ((Seq.length (xs)) - 1) then
                s <- s + " "
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
        printfn "%s" ("Perimeter format equality checks:")
        printfn "%s" ("  Q == R is " + (string (perimEqual (unbox<int array> [|8; 1; 3|]) (unbox<int array> [|1; 3; 8|]))))
        printfn "%s" ("  U == V is " + (string (perimEqual (unbox<int array> [|18; 8; 14; 10; 12; 17; 19|]) (unbox<int array> [|8; 14; 10; 12; 17; 19; 18|]))))
        let e: Edge array = [|{ a = 7; b = 11 }; { a = 1; b = 11 }; { a = 1; b = 7 }|]
        let f: Edge array = [|{ a = 11; b = 23 }; { a = 1; b = 17 }; { a = 17; b = 23 }; { a = 1; b = 11 }|]
        let g: Edge array = [|{ a = 8; b = 14 }; { a = 17; b = 19 }; { a = 10; b = 12 }; { a = 10; b = 14 }; { a = 12; b = 17 }; { a = 8; b = 18 }; { a = 18; b = 19 }|]
        let h: Edge array = [|{ a = 1; b = 3 }; { a = 9; b = 11 }; { a = 3; b = 11 }; { a = 1; b = 11 }|]
        printfn "%s" ("\nEdge to perimeter format translations:")
        let mutable faces: Edge array array = [|e; f; g; h|]
        let mutable names: string array = [|"E"; "F"; "G"; "H"|]
        let mutable idx: int = 0
        while idx < (Seq.length (faces)) do
            let per: obj = faceToPerim (_idx faces (idx))
            if per = null then
                printfn "%s" (("  " + (_idx names (idx))) + " => Invalid edge format")
            else
                printfn "%s" ((("  " + (_idx names (idx))) + " => ") + (listStr (unbox<int array> per)))
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
