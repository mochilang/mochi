// Generated 2025-08-08 16:34 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let ASCII: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
let rec build_alphabet () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        let mutable result: string array = [||]
        let mutable i: int = 0
        while i < (String.length (ASCII)) do
            result <- Array.append result [|(string (ASCII.[i]))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec range_list (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable lst: int array = [||]
        let mutable i: int = 0
        while i < n do
            lst <- Array.append lst [|i|]
            i <- i + 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
let rec reversed_range_list (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable lst: int array = [||]
        let mutable i: int = n - 1
        while i >= 0 do
            lst <- Array.append lst [|i|]
            i <- i - 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of_char (lst: string array) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of_int (lst: int array) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) = value then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec enigma_encrypt (message: string) (token: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable token = token
    try
        let alphabets: string array = build_alphabet()
        let n: int = Seq.length (alphabets)
        let mutable gear_one: int array = range_list (n)
        let mutable gear_two: int array = range_list (n)
        let mutable gear_three: int array = range_list (n)
        let reflector: int array = reversed_range_list (n)
        let mutable gear_one_pos: int = 0
        let mutable gear_two_pos: int = 0
        let mutable gear_three_pos: int = 0
        let rec rotator () =
            let mutable __ret = ()
            try
                let mutable i: int = _idx gear_one (0)
                gear_one <- Array.sub gear_one 1 ((Seq.length (gear_one)) - 1)
                gear_one <- Array.append gear_one [|i|]
                gear_one_pos <- gear_one_pos + 1
                if (((gear_one_pos % n + n) % n)) = 0 then
                    i <- _idx gear_two (0)
                    gear_two <- Array.sub gear_two 1 ((Seq.length (gear_two)) - 1)
                    gear_two <- Array.append gear_two [|i|]
                    gear_two_pos <- gear_two_pos + 1
                    if (((gear_two_pos % n + n) % n)) = 0 then
                        i <- _idx gear_three (0)
                        gear_three <- Array.sub gear_three 1 ((Seq.length (gear_three)) - 1)
                        gear_three <- Array.append gear_three [|i|]
                        gear_three_pos <- gear_three_pos + 1
                __ret
            with
                | Return -> __ret
        let rec engine (ch: string) =
            let mutable __ret : string = Unchecked.defaultof<string>
            let mutable ch = ch
            try
                let mutable target: int = index_of_char (alphabets) (ch)
                target <- _idx gear_one (target)
                target <- _idx gear_two (target)
                target <- _idx gear_three (target)
                target <- _idx reflector (target)
                target <- index_of_int (gear_three) (target)
                target <- index_of_int (gear_two) (target)
                target <- index_of_int (gear_one) (target)
                rotator()
                __ret <- _idx alphabets (target)
                raise Return
                __ret
            with
                | Return -> __ret
        let mutable t: int = 0
        while t < token do
            rotator()
            t <- t + 1
        let mutable result: string = ""
        let mutable idx: int = 0
        while idx < (String.length (message)) do
            result <- result + (unbox<string> (engine (string (message.[idx]))))
            idx <- idx + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let message: string = "HELLO WORLD"
let token: int = 123
let encoded: string = enigma_encrypt (message) (token)
printfn "%s" (encoded)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
