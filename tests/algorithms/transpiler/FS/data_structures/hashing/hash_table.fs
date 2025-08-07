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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type HashTable = {
    size_table: int
    values: int array
    filled: bool array
    charge_factor: int
    lim_charge: float
}
let rec repeat_int (n: int) (``val``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable ``val`` = ``val``
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|``val``|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and repeat_bool (n: int) (``val``: bool) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    let mutable ``val`` = ``val``
    try
        let mutable res: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|``val``|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and set_int (xs: int array) (idx: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and set_bool (xs: bool array) (idx: int) (value: bool) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable res: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and create_table (size_table: int) (charge_factor: int) (lim_charge: float) =
    let mutable __ret : HashTable = Unchecked.defaultof<HashTable>
    let mutable size_table = size_table
    let mutable charge_factor = charge_factor
    let mutable lim_charge = lim_charge
    try
        __ret <- { size_table = size_table; values = repeat_int (size_table) (0); filled = repeat_bool (size_table) (false); charge_factor = charge_factor; lim_charge = lim_charge }
        raise Return
        __ret
    with
        | Return -> __ret
and hash_function (ht: HashTable) (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ht = ht
    let mutable key = key
    try
        let mutable k: int = ((key % (ht.size_table) + (ht.size_table)) % (ht.size_table))
        if k < 0 then
            k <- k + (ht.size_table)
        __ret <- k
        raise Return
        __ret
    with
        | Return -> __ret
and is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        let mutable i: int = 3
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and next_prime (value: int) (factor: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    let mutable factor = factor
    try
        let mutable candidate: int = (value * factor) + 1
        while not (is_prime (candidate)) do
            candidate <- candidate + 1
        __ret <- candidate
        raise Return
        __ret
    with
        | Return -> __ret
and set_value (ht: HashTable) (key: int) (data: int) =
    let mutable __ret : HashTable = Unchecked.defaultof<HashTable>
    let mutable ht = ht
    let mutable key = key
    let mutable data = data
    try
        let new_values: int array = set_int (ht.values) (key) (data)
        let new_filled: bool array = set_bool (ht.filled) (key) (true)
        __ret <- { size_table = ht.size_table; values = new_values; filled = new_filled; charge_factor = ht.charge_factor; lim_charge = ht.lim_charge }
        raise Return
        __ret
    with
        | Return -> __ret
and collision_resolution (ht: HashTable) (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ht = ht
    let mutable key = key
    try
        let mutable new_key: int = hash_function (ht) (key + 1)
        let mutable steps: int = 0
        while _idx (ht.filled) (new_key) do
            new_key <- hash_function (ht) (new_key + 1)
            steps <- steps + 1
            if steps >= (ht.size_table) then
                __ret <- -1
                raise Return
        __ret <- new_key
        raise Return
        __ret
    with
        | Return -> __ret
and rehashing (ht: HashTable) =
    let mutable __ret : HashTable = Unchecked.defaultof<HashTable>
    let mutable ht = ht
    try
        let mutable survivors: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (ht.values)) do
            if _idx (ht.filled) (i) then
                survivors <- Array.append survivors [|_idx (ht.values) (i)|]
            i <- i + 1
        let new_size: int = next_prime (ht.size_table) (2)
        let mutable new_ht: HashTable = create_table (new_size) (ht.charge_factor) (ht.lim_charge)
        i <- 0
        while i < (Seq.length (survivors)) do
            new_ht <- insert_data (new_ht) (_idx survivors (i))
            i <- i + 1
        __ret <- new_ht
        raise Return
        __ret
    with
        | Return -> __ret
and insert_data (ht: HashTable) (data: int) =
    let mutable __ret : HashTable = Unchecked.defaultof<HashTable>
    let mutable ht = ht
    let mutable data = data
    try
        let key: int = hash_function (ht) (data)
        if not (_idx (ht.filled) (key)) then
            __ret <- set_value (ht) (key) (data)
            raise Return
        if (_idx (ht.values) (key)) = data then
            __ret <- ht
            raise Return
        let mutable new_key: int = collision_resolution (ht) (key)
        if new_key >= 0 then
            __ret <- set_value (ht) (new_key) (data)
            raise Return
        let resized: HashTable = rehashing (ht)
        __ret <- insert_data (resized) (data)
        raise Return
        __ret
    with
        | Return -> __ret
and keys (ht: HashTable) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable ht = ht
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (ht.values)) do
            if _idx (ht.filled) (i) then
                res <- Array.append res [|[|i; _idx (ht.values) (i)|]|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable ht: HashTable = create_table (3) (1) (0.75)
        ht <- insert_data (ht) (17)
        ht <- insert_data (ht) (18)
        ht <- insert_data (ht) (99)
        printfn "%s" (_repr (keys (ht)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
