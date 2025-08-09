// Generated 2025-08-09 10:14 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_avg (number_1: int) (number_2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number_1 = number_1
    let mutable number_2 = number_2
    try
        __ret <- _floordiv (number_1 + number_2) 2
        raise Return
        __ret
    with
        | Return -> __ret
let rec guess_the_number (lower: int) (higher: int) (to_guess: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable lower = lower
    let mutable higher = higher
    let mutable to_guess = to_guess
    try
        if lower > higher then
            failwith ("argument value for lower and higher must be(lower > higher)")
        if not ((lower < to_guess) && (to_guess < higher)) then
            failwith ("guess value must be within the range of lower and higher value")
        let rec answer (number: int) =
            let mutable __ret : string = Unchecked.defaultof<string>
            let mutable number = number
            try
                if number > to_guess then
                    __ret <- "high"
                    raise Return
                else
                    if number < to_guess then
                        __ret <- "low"
                        raise Return
                    else
                        __ret <- "same"
                        raise Return
                __ret
            with
                | Return -> __ret
        printfn "%s" ("started...")
        let mutable last_lowest: int = lower
        let mutable last_highest: int = higher
        let mutable last_numbers: int array = Array.empty<int>
        try
            while true do
                try
                    let number: int = get_avg (last_lowest) (last_highest)
                    last_numbers <- Array.append last_numbers [|number|]
                    let resp: string = answer (number)
                    if resp = "low" then
                        last_lowest <- number
                    else
                        if resp = "high" then
                            last_highest <- number
                        else
                            raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" ("guess the number : " + (_str (_idx last_numbers (int ((Seq.length (last_numbers)) - 1)))))
        printfn "%s" ("details : " + (_str (last_numbers)))
        __ret <- last_numbers
        raise Return
        __ret
    with
        | Return -> __ret
guess_the_number (10) (1000) (17)
guess_the_number (-10000) (10000) (7)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
