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
type KMeansResult = {
    centroids: float array array
    assignments: int array
}
let rec distance_sq (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable diff: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and mean (vectors: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vectors = vectors
    try
        let mutable dim: int = Seq.length (_idx vectors (0))
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < dim do
            let mutable total: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (vectors)) do
                total <- total + (_idx (_idx vectors (j)) (i))
                j <- j + 1
            res <- Array.append res [|total / (float (Seq.length (vectors)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and k_means (vectors: float array array) (k: int) (iterations: int) =
    let mutable __ret : KMeansResult = Unchecked.defaultof<KMeansResult>
    let mutable vectors = vectors
    let mutable k = k
    let mutable iterations = iterations
    try
        let mutable centroids: float array array = [||]
        let mutable i: int = 0
        while i < k do
            centroids <- Array.append centroids [|_idx vectors (i)|]
            i <- i + 1
        let mutable assignments: int array = [||]
        let mutable n: int = Seq.length (vectors)
        i <- 0
        while i < n do
            assignments <- Array.append assignments [|0|]
            i <- i + 1
        let mutable it: int = 0
        while it < iterations do
            let mutable v: int = 0
            while v < n do
                let mutable best: int = 0
                let mutable bestDist: float = distance_sq (_idx vectors (v)) (_idx centroids (0))
                let mutable c: int = 1
                while c < k do
                    let mutable d: float = distance_sq (_idx vectors (v)) (_idx centroids (c))
                    if d < bestDist then
                        bestDist <- d
                        best <- c
                    c <- c + 1
                assignments.[v] <- best
                v <- v + 1
            let mutable cIdx: int = 0
            while cIdx < k do
                let mutable cluster: float array array = [||]
                let mutable v2: int = 0
                while v2 < n do
                    if (_idx assignments (v2)) = cIdx then
                        cluster <- Array.append cluster [|_idx vectors (v2)|]
                    v2 <- v2 + 1
                if (Seq.length (cluster)) > 0 then
                    centroids.[cIdx] <- mean (cluster)
                cIdx <- cIdx + 1
            it <- it + 1
        __ret <- { centroids = centroids; assignments = assignments }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let vectors: float array array = [|[|1.0; 2.0|]; [|1.5; 1.8|]; [|5.0; 8.0|]; [|8.0; 8.0|]; [|1.0; 0.6|]; [|9.0; 11.0|]|]
        let result: KMeansResult = k_means (vectors) (2) (5)
        printfn "%s" (_str (result.centroids))
        printfn "%s" (_str (result.assignments))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
