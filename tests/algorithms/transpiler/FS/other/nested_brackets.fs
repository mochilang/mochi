// Generated 2025-08-09 16:21 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
open System.Collections.Generic

let OPEN_TO_CLOSED: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("(", ")"); ("[", "]"); ("{", "}")]
let rec slice_without_last (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and is_balanced (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable stack: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (s)) do
            let symbol: string = _substring s i (i + 1)
            if OPEN_TO_CLOSED.ContainsKey(symbol) then
                stack <- Array.append stack [|symbol|]
            else
                if ((symbol = ")") || (symbol = "]")) || (symbol = "}") then
                    if (Seq.length (stack)) = 0 then
                        __ret <- false
                        raise Return
                    let top: string = _idx stack (int ((Seq.length (stack)) - 1))
                    if (_dictGet OPEN_TO_CLOSED ((string (top)))) <> symbol then
                        __ret <- false
                        raise Return
                    stack <- slice_without_last (stack)
            i <- i + 1
        __ret <- (Seq.length (stack)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%b" (is_balanced (""))
        printfn "%b" (is_balanced ("()"))
        printfn "%b" (is_balanced ("[]"))
        printfn "%b" (is_balanced ("{}"))
        printfn "%b" (is_balanced ("()[]{}"))
        printfn "%b" (is_balanced ("(())"))
        printfn "%b" (is_balanced ("[["))
        printfn "%b" (is_balanced ("([{}])"))
        printfn "%b" (is_balanced ("(()[)]"))
        printfn "%b" (is_balanced ("([)]"))
        printfn "%b" (is_balanced ("[[()]]"))
        printfn "%b" (is_balanced ("(()(()))"))
        printfn "%b" (is_balanced ("]"))
        printfn "%b" (is_balanced ("Life is a bowl of cherries."))
        printfn "%b" (is_balanced ("Life is a bowl of che{}ies."))
        printfn "%b" (is_balanced ("Life is a bowl of che}{ies."))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
