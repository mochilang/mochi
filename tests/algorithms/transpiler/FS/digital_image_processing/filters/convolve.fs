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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pad_edge (image: int array array) (pad_size: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable pad_size = pad_size
    try
        let height: int = Seq.length (image)
        let width: int = Seq.length (_idx image (0))
        let new_height: int = height + (pad_size * 2)
        let new_width: int = width + (pad_size * 2)
        let mutable padded: int array array = [||]
        let mutable i: int = 0
        while i < new_height do
            let mutable row: int array = [||]
            let mutable src_i: int = i
            if src_i < pad_size then
                src_i <- 0
            if src_i >= (height + pad_size) then
                src_i <- height - 1
            else
                src_i <- src_i - pad_size
            let mutable j: int = 0
            while j < new_width do
                let mutable src_j: int = j
                if src_j < pad_size then
                    src_j <- 0
                if src_j >= (width + pad_size) then
                    src_j <- width - 1
                else
                    src_j <- src_j - pad_size
                row <- Array.append row [|_idx (_idx image (src_i)) (src_j)|]
                j <- j + 1
            padded <- Array.append padded [|row|]
            i <- i + 1
        __ret <- padded
        raise Return
        __ret
    with
        | Return -> __ret
let rec im2col (image: int array array) (block_h: int) (block_w: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable block_h = block_h
    let mutable block_w = block_w
    try
        let rows: int = Seq.length (image)
        let cols: int = Seq.length (_idx image (0))
        let dst_height: int = (rows - block_h) + 1
        let dst_width: int = (cols - block_w) + 1
        let mutable image_array: int array array = [||]
        let mutable i: int = 0
        while i < dst_height do
            let mutable j: int = 0
            while j < dst_width do
                let mutable window: int array = [||]
                let mutable bi: int = 0
                while bi < block_h do
                    let mutable bj: int = 0
                    while bj < block_w do
                        window <- Array.append window [|_idx (_idx image (i + bi)) (j + bj)|]
                        bj <- bj + 1
                    bi <- bi + 1
                image_array <- Array.append image_array [|window|]
                j <- j + 1
            i <- i + 1
        __ret <- image_array
        raise Return
        __ret
    with
        | Return -> __ret
let rec flatten (matrix: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable matrix = matrix
    try
        let mutable out: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (matrix)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx matrix (i))) do
                out <- Array.append out [|_idx (_idx matrix (i)) (j)|]
                j <- j + 1
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: int array) (b: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- sum + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec img_convolve (image: int array array) (kernel: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable kernel = kernel
    try
        let height: int = Seq.length (image)
        let width: int = Seq.length (_idx image (0))
        let k_size: int = Seq.length (kernel)
        let pad_size: int = k_size / 2
        let mutable padded: int array array = pad_edge (image) (pad_size)
        let mutable image_array: int array array = im2col (padded) (k_size) (k_size)
        let kernel_flat: int array = flatten (kernel)
        let mutable dst: int array array = [||]
        let mutable idx: int = 0
        let mutable i: int = 0
        while i < height do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < width do
                let ``val``: int = dot (_idx image_array (idx)) (kernel_flat)
                row <- Array.append row [|``val``|]
                idx <- idx + 1
                j <- j + 1
            dst <- Array.append dst [|row|]
            i <- i + 1
        __ret <- dst
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_matrix (m: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx m (i))) do
                if j > 0 then
                    line <- line + " "
                line <- line + (_str (_idx (_idx m (i)) (j)))
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let image: int array array = [|[|1; 2; 3; 0; 0|]; [|4; 5; 6; 0; 0|]; [|7; 8; 9; 0; 0|]; [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]|]
let laplace_kernel: int array array = [|[|0; 1; 0|]; [|1; -4; 1|]; [|0; 1; 0|]|]
let result: int array array = img_convolve (image) (laplace_kernel)
print_matrix (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
