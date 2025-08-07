// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Bloom = {
    size: int
    bits: int array
}
let ascii: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (ascii)) do
            if (ascii.Substring(i, (i + 1) - i)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and new_bloom (size: int) =
    let mutable __ret : Bloom = Unchecked.defaultof<Bloom>
    let mutable size = size
    try
        let mutable bits: int array = [||]
        let mutable i: int = 0
        while i < size do
            bits <- Array.append bits [|0|]
            i <- i + 1
        __ret <- { size = size; bits = bits }
        raise Return
        __ret
    with
        | Return -> __ret
and hash1 (value: string) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    let mutable size = size
    try
        let mutable h: int = 0
        let mutable i: int = 0
        while i < (String.length (value)) do
            h <- ((((h * 31) + (ord (value.Substring(i, (i + 1) - i)))) % size + size) % size)
            i <- i + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and hash2 (value: string) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    let mutable size = size
    try
        let mutable h: int = 0
        let mutable i: int = 0
        while i < (String.length (value)) do
            h <- ((((h * 131) + (ord (value.Substring(i, (i + 1) - i)))) % size + size) % size)
            i <- i + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and hash_positions (value: string) (size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable value = value
    let mutable size = size
    try
        let h1: int = hash1 (value) (size)
        let h2: int = hash2 (value) (size)
        let mutable res: int array = [||]
        res <- Array.append res [|h1|]
        res <- Array.append res [|h2|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bloom_add (b: Bloom) (value: string) =
    let mutable __ret : Bloom = Unchecked.defaultof<Bloom>
    let mutable b = b
    let mutable value = value
    try
        let pos: int array = hash_positions (value) (b.size)
        let mutable bits: int array = b.bits
        let mutable i: int = 0
        while i < (Seq.length (pos)) do
            let idx: int = ((b.size) - 1) - (_idx pos (i))
            bits.[idx] <- 1
            i <- i + 1
        __ret <- { size = b.size; bits = bits }
        raise Return
        __ret
    with
        | Return -> __ret
and bloom_exists (b: Bloom) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable value = value
    try
        let pos: int array = hash_positions (value) (b.size)
        let mutable i: int = 0
        while i < (Seq.length (pos)) do
            let idx: int = ((b.size) - 1) - (_idx pos (i))
            if (_idx (b.bits) (idx)) <> 1 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and bitstring (b: Bloom) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (b.size) do
            res <- res + (_str (_idx (b.bits) (i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and format_hash (b: Bloom) (value: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    let mutable value = value
    try
        let pos: int array = hash_positions (value) (b.size)
        let mutable bits: int array = [||]
        let mutable i: int = 0
        while i < (b.size) do
            bits <- Array.append bits [|0|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (pos)) do
            let idx: int = ((b.size) - 1) - (_idx pos (i))
            bits.[idx] <- 1
            i <- i + 1
        let mutable res: string = ""
        i <- 0
        while i < (b.size) do
            res <- res + (_str (_idx bits (i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and estimated_error_rate (b: Bloom) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b = b
    try
        let mutable ones: int = 0
        let mutable i: int = 0
        while i < (b.size) do
            if (_idx (b.bits) (i)) = 1 then
                ones <- ones + 1
            i <- i + 1
        let frac: float = (float ones) / (float (b.size))
        __ret <- frac * frac
        raise Return
        __ret
    with
        | Return -> __ret
and any_in (b: Bloom) (items: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable items = items
    try
        let mutable i: int = 0
        while i < (Seq.length (items)) do
            if bloom_exists (b) (_idx items (i)) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable bloom: Bloom = new_bloom (8)
        printfn "%s" (bitstring (bloom))
        printfn "%b" (bloom_exists (bloom) ("Titanic"))
        bloom <- bloom_add (bloom) ("Titanic")
        printfn "%s" (bitstring (bloom))
        printfn "%b" (bloom_exists (bloom) ("Titanic"))
        bloom <- bloom_add (bloom) ("Avatar")
        printfn "%b" (bloom_exists (bloom) ("Avatar"))
        printfn "%s" (format_hash (bloom) ("Avatar"))
        printfn "%s" (bitstring (bloom))
        let not_present: string array = [|"The Godfather"; "Interstellar"; "Parasite"; "Pulp Fiction"|]
        let mutable i: int = 0
        while i < (Seq.length (not_present)) do
            let film: string = _idx not_present (i)
            printfn "%s" ((film + ":") + (format_hash (bloom) (film)))
            i <- i + 1
        printfn "%b" (any_in (bloom) (not_present))
        printfn "%b" (bloom_exists (bloom) ("Ratatouille"))
        printfn "%s" (format_hash (bloom) ("Ratatouille"))
        printfn "%s" (_str (estimated_error_rate (bloom)))
        bloom <- bloom_add (bloom) ("The Godfather")
        printfn "%s" (_str (estimated_error_rate (bloom)))
        printfn "%s" (bitstring (bloom))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
