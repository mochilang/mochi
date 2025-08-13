// Generated 2025-08-13 07:12 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec key (state: string) (obs: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable state = state
    let mutable obs = obs
    try
        __ret <- (state + "|") + obs
        raise Return
        __ret
    with
        | Return -> __ret
and viterbi (observations: string array) (states: string array) (start_p: System.Collections.Generic.IDictionary<string, float>) (trans_p: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, float>>) (emit_p: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, float>>) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable observations = observations
    let mutable states = states
    let mutable start_p = start_p
    let mutable trans_p = trans_p
    let mutable emit_p = emit_p
    try
        if ((Seq.length (observations)) = 0) || ((Seq.length (states)) = 0) then
            ignore (failwith ("empty parameters"))
        let mutable probs: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        let mutable ptrs: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        let first_obs: string = _idx observations (int 0)
        let mutable i: int = 0
        while i < (Seq.length (states)) do
            let state: string = _idx states (int i)
            probs <- _dictAdd (probs) (string (key (state) (first_obs))) ((_dictGet start_p ((string (state)))) * (_dictGet (_dictGet emit_p ((string (state)))) ((string (first_obs)))))
            ptrs <- _dictAdd (ptrs) (string (key (state) (first_obs))) ("")
            i <- i + 1
        let mutable t: int = 1
        while t < (Seq.length (observations)) do
            let obs: string = _idx observations (int t)
            let mutable j: int = 0
            while j < (Seq.length (states)) do
                let state: string = _idx states (int j)
                let mutable max_prob: float = -1.0
                let mutable prev_state: string = ""
                let mutable k: int = 0
                while k < (Seq.length (states)) do
                    let state0: string = _idx states (int k)
                    let obs0: string = _idx observations (int (t - 1))
                    let prob_prev: float = _dictGet probs ((string (key (state0) (obs0))))
                    let prob: float = (prob_prev * (_dictGet (_dictGet trans_p ((string (state0)))) ((string (state))))) * (_dictGet (_dictGet emit_p ((string (state)))) ((string (obs))))
                    if prob > max_prob then
                        max_prob <- prob
                        prev_state <- state0
                    k <- k + 1
                probs <- _dictAdd (probs) (string (key (state) (obs))) (max_prob)
                ptrs <- _dictAdd (ptrs) (string (key (state) (obs))) (prev_state)
                j <- j + 1
            t <- t + 1
        let mutable path: string array = Array.empty<string>
        let mutable n: int = 0
        while n < (Seq.length (observations)) do
            path <- Array.append path [|""|]
            n <- n + 1
        let last_obs: string = _idx observations (int ((Seq.length (observations)) - 1))
        let mutable max_final: float = -1.0
        let mutable last_state: string = ""
        let mutable m: int = 0
        while m < (Seq.length (states)) do
            let state: string = _idx states (int m)
            let prob: float = _dictGet probs ((string (key (state) (last_obs))))
            if prob > max_final then
                max_final <- prob
                last_state <- state
            m <- m + 1
        let last_index: int = (Seq.length (observations)) - 1
        path.[last_index] <- last_state
        let mutable idx: int = last_index
        while idx > 0 do
            let obs: string = _idx observations (int idx)
            let prev: string = _dictGet ptrs ((string (key (_idx path (int idx)) (obs))))
            path.[(idx - 1)] <- prev
            idx <- idx - 1
        __ret <- path
        raise Return
        __ret
    with
        | Return -> __ret
and join_words (words: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable words = words
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (words)) do
            if i > 0 then
                res <- res + " "
            res <- res + (_idx words (int i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable observations: string array = unbox<string array> [|"normal"; "cold"; "dizzy"|]
let mutable states: string array = unbox<string array> [|"Healthy"; "Fever"|]
let mutable start_p: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("Healthy", 0.6); ("Fever", 0.4)]
let mutable trans_p: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, float>> = _dictCreate [("Healthy", _dictCreate [("Healthy", 0.7); ("Fever", 0.3)]); ("Fever", _dictCreate [("Healthy", 0.4); ("Fever", 0.6)])]
let mutable emit_p: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, float>> = _dictCreate [("Healthy", _dictCreate [("normal", 0.5); ("cold", 0.4); ("dizzy", 0.1)]); ("Fever", _dictCreate [("normal", 0.1); ("cold", 0.3); ("dizzy", 0.6)])]
let result: string array = viterbi (observations) (states) (start_p) (trans_p) (emit_p)
ignore (printfn "%s" (join_words (result)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
