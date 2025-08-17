// Generated 2025-08-17 12:28 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
type QR = {
    mutable _q: float array array
    mutable _r: float array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and sign (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x >= 0.0 then
            __ret <- 1.0
            raise Return
        else
            __ret <- -1.0
            raise Return
        __ret
    with
        | Return -> __ret
and vector_norm (v: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            sum <- sum + ((_idx v (int i)) * (_idx v (int i)))
            i <- i + 1
        let n: float = sqrt_approx (sum)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and identity_matrix (n: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    try
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- mat
        raise Return
        __ret
    with
        | Return -> __ret
and copy_matrix (a: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    try
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx a (int i))) do
                row <- Array.append row [|(_idx (_idx a (int i)) (int j))|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- mat
        raise Return
        __ret
    with
        | Return -> __ret
and matmul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let m: int = Seq.length (a)
        let n: int = Seq.length (_idx a (int 0))
        let p: int = Seq.length (_idx b (int 0))
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < m do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < p do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < n do
                    sum <- sum + ((_idx (_idx a (int i)) (int k)) * (_idx (_idx b (int k)) (int j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and qr_decomposition (a: float array array) =
    let mutable __ret : QR = Unchecked.defaultof<QR>
    let mutable a = a
    try
        let m: int = Seq.length (a)
        let n: int = Seq.length (_idx a (int 0))
        let t: int = if m < n then m else n
        let mutable _q: float array array = identity_matrix (m)
        let mutable _r: float array array = copy_matrix (a)
        let mutable k: int = 0
        while k < (t - 1) do
            let mutable x: float array = Array.empty<float>
            let mutable i: int = k
            while i < m do
                x <- Array.append x [|(_idx (_idx _r (int i)) (int k))|]
                i <- i + 1
            let mutable e1: float array = Array.empty<float>
            i <- 0
            while i < (Seq.length (x)) do
                if i = 0 then
                    e1 <- Array.append e1 [|1.0|]
                else
                    e1 <- Array.append e1 [|0.0|]
                i <- i + 1
            let alpha: float = vector_norm (x)
            let s: float = (sign (_idx x (int 0))) * alpha
            let mutable v: float array = Array.empty<float>
            i <- 0
            while i < (Seq.length (x)) do
                v <- Array.append v [|((_idx x (int i)) + (s * (_idx e1 (int i))))|]
                i <- i + 1
            let vnorm: float = vector_norm (v)
            i <- 0
            while i < (Seq.length (v)) do
                v.[i] <- (_idx v (int i)) / vnorm
                i <- i + 1
            let size: int = Seq.length (v)
            let mutable qk_small: float array array = Array.empty<float array>
            i <- 0
            while i < size do
                let mutable row: float array = Array.empty<float>
                let mutable j: int = 0
                while j < size do
                    let delta: float = if i = j then 1.0 else 0.0
                    row <- Array.append row [|(delta - ((2.0 * (_idx v (int i))) * (_idx v (int j))))|]
                    j <- j + 1
                qk_small <- Array.append qk_small [|row|]
                i <- i + 1
            let mutable qk: float array array = identity_matrix (m)
            i <- 0
            while i < size do
                let mutable j: int = 0
                while j < size do
                    qk.[(k + i)].[(k + j)] <- _idx (_idx qk_small (int i)) (int j)
                    j <- j + 1
                i <- i + 1
            _q <- matmul (_q) (qk)
            _r <- matmul (qk) (_r)
            k <- k + 1
        __ret <- { _q = _q; _r = _r }
        raise Return
        __ret
    with
        | Return -> __ret
and print_matrix (mat: float array array) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (int i))) do
                line <- line + (_str (_idx (_idx mat (int i)) (int j)))
                if (j + 1) < (Seq.length (_idx mat (int i))) then
                    line <- line + " "
                j <- j + 1
            ignore (printfn "%s" (line))
            i <- i + 1
        __ret
    with
        | Return -> __ret
let A: float array array = [|[|12.0; -51.0; 4.0|]; [|6.0; 167.0; -68.0|]; [|-4.0; 24.0; -41.0|]|]
let result: QR = qr_decomposition (A)
ignore (print_matrix (result._q))
ignore (print_matrix (result._r))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
