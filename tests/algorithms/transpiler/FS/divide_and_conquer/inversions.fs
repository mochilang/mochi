// Generated 2025-08-07 15:46 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type InvResult = {
    arr: int array
    inv: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec slice_list (arr: int array) (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: int array = [||]
        let mutable k: int = start
        while k < ``end`` do
            res <- Array.append res [|_idx arr (k)|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_inversions_bf (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable inv: int = 0
        let mutable i: int = 0
        while i < (n - 1) do
            let mutable j: int = i + 1
            while j < n do
                if (_idx arr (i)) > (_idx arr (j)) then
                    inv <- inv + 1
                j <- j + 1
            i <- i + 1
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_cross_inversions (p: int array) (q: int array) =
    let mutable __ret : InvResult = Unchecked.defaultof<InvResult>
    let mutable p = p
    let mutable q = q
    try
        let mutable r: int array = [||]
        let mutable i: int = 0
        let mutable j: int = 0
        let mutable inv: int = 0
        while (i < (Seq.length (p))) && (j < (Seq.length (q))) do
            if (_idx p (i)) > (_idx q (j)) then
                inv <- inv + ((Seq.length (p)) - i)
                r <- Array.append r [|_idx q (j)|]
                j <- j + 1
            else
                r <- Array.append r [|_idx p (i)|]
                i <- i + 1
        if i < (Seq.length (p)) then
            r <- unbox<int array> (Array.append (r) (slice_list (p) (i) (Seq.length (p))))
        else
            r <- unbox<int array> (Array.append (r) (slice_list (q) (j) (Seq.length (q))))
        __ret <- { arr = r; inv = inv }
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_inversions_recursive (arr: int array) =
    let mutable __ret : InvResult = Unchecked.defaultof<InvResult>
    let mutable arr = arr
    try
        if (Seq.length (arr)) <= 1 then
            __ret <- { arr = arr; inv = 0 }
            raise Return
        let mid: int = (Seq.length (arr)) / 2
        let p: int array = slice_list (arr) (0) (mid)
        let q: int array = slice_list (arr) (mid) (Seq.length (arr))
        let res_p: InvResult = count_inversions_recursive (p)
        let res_q: InvResult = count_inversions_recursive (q)
        let res_cross: InvResult = count_cross_inversions (res_p.arr) (res_q.arr)
        let total: int = ((res_p.inv) + (res_q.inv)) + (res_cross.inv)
        __ret <- { arr = res_cross.arr; inv = total }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable arr_1: int array = [|10; 2; 1; 5; 5; 2; 11|]
let nbf: int = count_inversions_bf (arr_1)
let nrec: int = (count_inversions_recursive (arr_1)).inv
printfn "%s" (String.concat " " ([|sprintf "%s" ("number of inversions = "); sprintf "%d" (nbf)|]))
arr_1 <- unbox<int array> [|1; 2; 2; 5; 5; 10; 11|]
let nbf2: int = count_inversions_bf (arr_1)
let nrec2: int = (count_inversions_recursive (arr_1)).inv
printfn "%s" (String.concat " " ([|sprintf "%s" ("number of inversions = "); sprintf "%d" (nbf2)|]))
arr_1 <- Array.empty<int>
let nbf3: int = count_inversions_bf (arr_1)
let nrec3: int = (count_inversions_recursive (arr_1)).inv
printfn "%s" (String.concat " " ([|sprintf "%s" ("number of inversions = "); sprintf "%d" (nbf3)|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
