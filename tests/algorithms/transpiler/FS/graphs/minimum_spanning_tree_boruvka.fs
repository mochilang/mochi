// Generated 2025-08-08 16:03 +0700

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
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Edge = {
    mutable _u: int
    mutable _v: int
    mutable _w: int
}
type UF = {
    mutable _parent: int array
    mutable _rank: int array
}
type FindRes = {
    mutable _root: int
    mutable _uf: UF
}
let rec uf_make (n: int) =
    let mutable __ret : UF = Unchecked.defaultof<UF>
    let mutable n = n
    try
        let mutable p: int array = [||]
        let mutable r: int array = [||]
        let mutable i: int = 0
        while i < n do
            p <- Array.append p [|i|]
            r <- Array.append r [|0|]
            i <- i + 1
        __ret <- { _parent = p; _rank = r }
        raise Return
        __ret
    with
        | Return -> __ret
and uf_find (_uf: UF) (x: int) =
    let mutable __ret : FindRes = Unchecked.defaultof<FindRes>
    let mutable _uf = _uf
    let mutable x = x
    try
        let mutable p: int array = _uf._parent
        if (_idx p (x)) <> x then
            let res: FindRes = uf_find ({ _parent = p; _rank = _uf._rank }) (_idx p (x))
            p <- (res._uf)._parent
            p.[x] <- res._root
            __ret <- { _root = res._root; _uf = { _parent = p; _rank = (res._uf)._rank } }
            raise Return
        __ret <- { _root = x; _uf = _uf }
        raise Return
        __ret
    with
        | Return -> __ret
and uf_union (_uf: UF) (x: int) (y: int) =
    let mutable __ret : UF = Unchecked.defaultof<UF>
    let mutable _uf = _uf
    let mutable x = x
    let mutable y = y
    try
        let fr1: FindRes = uf_find (_uf) (x)
        let mutable uf1: UF = fr1._uf
        let root1: int = fr1._root
        let fr2: FindRes = uf_find (uf1) (y)
        uf1 <- fr2._uf
        let root2: int = fr2._root
        if root1 = root2 then
            __ret <- uf1
            raise Return
        let mutable p: int array = uf1._parent
        let mutable r: int array = uf1._rank
        if (_idx r (root1)) > (_idx r (root2)) then
            p.[root2] <- root1
        else
            if (_idx r (root1)) < (_idx r (root2)) then
                p.[root1] <- root2
            else
                p.[root2] <- root1
                r.[root1] <- (_idx r (root1)) + 1
        __ret <- { _parent = p; _rank = r }
        raise Return
        __ret
    with
        | Return -> __ret
and boruvka (n: int) (edges: Edge array) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable n = n
    let mutable edges = edges
    try
        let mutable _uf: UF = uf_make (n)
        let mutable num_components: int = n
        let mutable mst: Edge array = [||]
        while num_components > 1 do
            let mutable cheap: int array = [||]
            let mutable i: int = 0
            while i < n do
                cheap <- Array.append cheap [|(0 - 1)|]
                i <- i + 1
            let mutable idx: int = 0
            while idx < (Seq.length (edges)) do
                let e: Edge = _idx edges (idx)
                let fr1: FindRes = uf_find (_uf) (e._u)
                _uf <- fr1._uf
                let set1: int = fr1._root
                let fr2: FindRes = uf_find (_uf) (e._v)
                _uf <- fr2._uf
                let set2: int = fr2._root
                if set1 <> set2 then
                    if ((_idx cheap (set1)) = (0 - 1)) || (((_idx edges (_idx cheap (set1)))._w) > (e._w)) then
                        cheap.[set1] <- idx
                    if ((_idx cheap (set2)) = (0 - 1)) || (((_idx edges (_idx cheap (set2)))._w) > (e._w)) then
                        cheap.[set2] <- idx
                idx <- idx + 1
            let mutable _v: int = 0
            while _v < n do
                let idxe: int = _idx cheap (_v)
                if idxe <> (0 - 1) then
                    let e: Edge = _idx edges (idxe)
                    let fr1: FindRes = uf_find (_uf) (e._u)
                    _uf <- fr1._uf
                    let set1: int = fr1._root
                    let fr2: FindRes = uf_find (_uf) (e._v)
                    _uf <- fr2._uf
                    let set2: int = fr2._root
                    if set1 <> set2 then
                        mst <- Array.append mst [|e|]
                        _uf <- uf_union (_uf) (set1) (set2)
                        num_components <- num_components - 1
                _v <- _v + 1
        __ret <- mst
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let edges: Edge array = [|{ _u = 0; _v = 1; _w = 1 }; { _u = 0; _v = 2; _w = 2 }; { _u = 2; _v = 3; _w = 3 }|]
        let mutable mst: Edge array = boruvka (4) (edges)
        for e in mst do
            printfn "%s" (((((_str (e._u)) + " - ") + (_str (e._v))) + " : ") + (_str (e._w)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
