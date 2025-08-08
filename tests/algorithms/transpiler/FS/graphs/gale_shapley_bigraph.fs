// Generated 2025-08-08 16:03 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec index_of (xs: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_item (xs: int array) (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable res: int array = [||]
        let mutable removed: bool = false
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (not removed) && ((_idx xs (i)) = x) then
                removed <- true
            else
                res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec stable_matching (donor_pref: int array array) (recipient_pref: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable donor_pref = donor_pref
    let mutable recipient_pref = recipient_pref
    try
        if (Seq.length (donor_pref)) <> (Seq.length (recipient_pref)) then
            failwith ("unequal groups")
        let n: int = Seq.length (donor_pref)
        let mutable unmatched: int array = [||]
        let mutable i: int = 0
        while i < n do
            unmatched <- Array.append unmatched [|i|]
            i <- i + 1
        let mutable donor_record: int array = [||]
        i <- 0
        while i < n do
            donor_record <- Array.append donor_record [|(-1)|]
            i <- i + 1
        let mutable rec_record: int array = [||]
        i <- 0
        while i < n do
            rec_record <- Array.append rec_record [|(-1)|]
            i <- i + 1
        let mutable num_donations: int array = [||]
        i <- 0
        while i < n do
            num_donations <- Array.append num_donations [|0|]
            i <- i + 1
        while (Seq.length (unmatched)) > 0 do
            let donor: int = _idx unmatched (0)
            let donor_preference: int array = _idx donor_pref (donor)
            let recipient: int = _idx donor_preference (_idx num_donations (donor))
            num_donations.[donor] <- (_idx num_donations (donor)) + 1
            let rec_preference: int array = _idx recipient_pref (recipient)
            let prev_donor: int = _idx rec_record (recipient)
            if prev_donor <> (0 - 1) then
                let prev_index: int = index_of (rec_preference) (prev_donor)
                let new_index: int = index_of (rec_preference) (donor)
                if prev_index > new_index then
                    rec_record.[recipient] <- donor
                    donor_record.[donor] <- recipient
                    unmatched <- Array.append unmatched [|prev_donor|]
                    unmatched <- remove_item (unmatched) (donor)
            else
                rec_record.[recipient] <- donor
                donor_record.[donor] <- recipient
                unmatched <- remove_item (unmatched) (donor)
        __ret <- donor_record
        raise Return
        __ret
    with
        | Return -> __ret
let donor_pref: int array array = [|[|0; 1; 3; 2|]; [|0; 2; 3; 1|]; [|1; 0; 2; 3|]; [|0; 3; 1; 2|]|]
let recipient_pref: int array array = [|[|3; 1; 2; 0|]; [|3; 1; 0; 2|]; [|0; 3; 1; 2|]; [|1; 0; 3; 2|]|]
printfn "%s" (_str (stable_matching (donor_pref) (recipient_pref)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
