// Generated 2025-08-12 13:41 +0700

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

open System

open System.IO

let rec card_value (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        if c = "2" then
            __ret <- 2
            raise Return
        if c = "3" then
            __ret <- 3
            raise Return
        if c = "4" then
            __ret <- 4
            raise Return
        if c = "5" then
            __ret <- 5
            raise Return
        if c = "6" then
            __ret <- 6
            raise Return
        if c = "7" then
            __ret <- 7
            raise Return
        if c = "8" then
            __ret <- 8
            raise Return
        if c = "9" then
            __ret <- 9
            raise Return
        if c = "T" then
            __ret <- 10
            raise Return
        if c = "J" then
            __ret <- 11
            raise Return
        if c = "Q" then
            __ret <- 12
            raise Return
        if c = "K" then
            __ret <- 13
            raise Return
        if c = "A" then
            __ret <- 14
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and sort_desc (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable n: int = Seq.length (arr)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (_idx arr (int j)) < (_idx arr (int (j + 1))) then
                    let tmp: int = _idx arr (int j)
                    arr.[j] <- _idx arr (int (j + 1))
                    arr.[(j + 1)] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and filter_not (xs: int array) (v: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable v = v
    try
        let mutable res: int array = Array.empty<int>
        for x in xs do
            if x <> v then
                res <- Array.append res [|x|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and hand_rank (hand: string array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable hand = hand
    try
        let mutable ranks: int array = Array.empty<int>
        let mutable suits: string array = Array.empty<string>
        for card in Seq.map string (hand) do
            ranks <- Array.append ranks [|(card_value (string (card.[0])))|]
            suits <- Array.append suits [|(string (card.[1]))|]
        ranks <- sort_desc (ranks)
        let mutable flush: bool = true
        let mutable i: int = 1
        while i < (Seq.length (suits)) do
            if (_idx suits (int i)) <> (_idx suits (int 0)) then
                flush <- false
            i <- i + 1
        let mutable straight: bool = true
        i <- 0
        while i < 4 do
            if ((_idx ranks (int i)) - (_idx ranks (int (i + 1)))) <> 1 then
                straight <- false
            i <- i + 1
        if (((((not straight) && ((_idx ranks (int 0)) = 14)) && ((_idx ranks (int 1)) = 5)) && ((_idx ranks (int 2)) = 4)) && ((_idx ranks (int 3)) = 3)) && ((_idx ranks (int 4)) = 2) then
            straight <- true
            ranks <- unbox<int array> [|5; 4; 3; 2; 1|]
        let mutable counts: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        for r in ranks do
            if counts.ContainsKey(r) then
                counts <- _dictAdd (counts) (r) ((_dictGet counts (r)) + 1)
            else
                counts <- _dictAdd (counts) (r) (1)
        let mutable uniq: int array = Array.empty<int>
        for r in ranks do
            let mutable exists: bool = false
            for u in uniq do
                if u = r then
                    exists <- true
            if not exists then
                uniq <- Array.append uniq [|r|]
        let mutable count_vals: int array array = Array.empty<int array>
        for u in uniq do
            count_vals <- Array.append count_vals [|[|_dictGet counts (u); u|]|]
        let mutable n: int = Seq.length (count_vals)
        let mutable i2: int = 0
        while i2 < n do
            let mutable j2: int = 0
            while j2 < (n - 1) do
                let a: int array = _idx count_vals (int j2)
                let b: int array = _idx count_vals (int (j2 + 1))
                if ((_idx a (int 0)) < (_idx b (int 0))) || (((_idx a (int 0)) = (_idx b (int 0))) && ((_idx a (int 1)) < (_idx b (int 1)))) then
                    let tmp: int array = _idx count_vals (int j2)
                    count_vals.[j2] <- _idx count_vals (int (j2 + 1))
                    count_vals.[(j2 + 1)] <- tmp
                j2 <- j2 + 1
            i2 <- i2 + 1
        let c1: int = _idx (_idx count_vals (int 0)) (int 0)
        let v1: int = _idx (_idx count_vals (int 0)) (int 1)
        let mutable rank: int array = Array.empty<int>
        if straight && flush then
            if (_idx ranks (int 0)) = 14 then
                rank <- Array.append rank [|9|]
                rank <- Array.append rank [|14|]
                __ret <- rank
                raise Return
            rank <- Array.append rank [|8|]
            rank <- Array.append rank [|(_idx ranks (int 0))|]
            __ret <- rank
            raise Return
        if c1 = 4 then
            let mutable kicker: int = 0
            for r in ranks do
                if r <> v1 then
                    kicker <- r
            rank <- Array.append rank [|7|]
            rank <- Array.append rank [|v1|]
            rank <- Array.append rank [|kicker|]
            __ret <- rank
            raise Return
        if c1 = 3 then
            let c2: int = _idx (_idx count_vals (int 1)) (int 0)
            let v2: int = _idx (_idx count_vals (int 1)) (int 1)
            if c2 = 2 then
                rank <- Array.append rank [|6|]
                rank <- Array.append rank [|v1|]
                rank <- Array.append rank [|v2|]
                __ret <- rank
                raise Return
            rank <- Array.append rank [|3|]
            rank <- Array.append rank [|v1|]
            for r in ranks do
                if r <> v1 then
                    rank <- Array.append rank [|r|]
            __ret <- rank
            raise Return
        if c1 = 2 then
            let c2: int = _idx (_idx count_vals (int 1)) (int 0)
            let v2: int = _idx (_idx count_vals (int 1)) (int 1)
            if c2 = 2 then
                let mutable high_pair: int = v1
                let mutable low_pair: int = v2
                if low_pair > high_pair then
                    let tmp: int = high_pair
                    high_pair <- low_pair
                    low_pair <- tmp
                let mutable kicker: int = 0
                for r in ranks do
                    if (r <> high_pair) && (r <> low_pair) then
                        kicker <- r
                rank <- Array.append rank [|2|]
                rank <- Array.append rank [|high_pair|]
                rank <- Array.append rank [|low_pair|]
                rank <- Array.append rank [|kicker|]
                __ret <- rank
                raise Return
            rank <- Array.append rank [|1|]
            rank <- Array.append rank [|v1|]
            for r in ranks do
                if r <> v1 then
                    rank <- Array.append rank [|r|]
            __ret <- rank
            raise Return
        if flush then
            rank <- Array.append rank [|5|]
            for r in ranks do
                rank <- Array.append rank [|r|]
            __ret <- rank
            raise Return
        if straight then
            rank <- Array.append rank [|4|]
            rank <- Array.append rank [|(_idx ranks (int 0))|]
            __ret <- rank
            raise Return
        rank <- Array.append rank [|0|]
        for r in ranks do
            rank <- Array.append rank [|r|]
        __ret <- rank
        raise Return
        __ret
    with
        | Return -> __ret
and compare_hands (h1: string array) (h2: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h1 = h1
    let mutable h2 = h2
    try
        let r1: int array = hand_rank (h1)
        let r2: int array = hand_rank (h2)
        let mutable i: int = 0
        while (i < (Seq.length (r1))) && (i < (Seq.length (r2))) do
            if (_idx r1 (int i)) > (_idx r2 (int i)) then
                __ret <- 1
                raise Return
            if (_idx r1 (int i)) < (_idx r2 (int i)) then
                __ret <- -1
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let hands: System.Collections.Generic.IDictionary<string, string> array = (File.ReadAllLines("/workspace/mochi/tests/github/TheAlgorithms/Python/project_euler/problem_054/poker_hands.txt") |> Array.map (fun line ->
        let cols = line.Split(" ")
        let d = System.Collections.Generic.Dictionary<string,string>()
        for i in 0 .. cols.Length - 1 do
            d.Add(sprintf "c%d" i, cols.[i])
        upcast d : System.Collections.Generic.IDictionary<string,string>))
        let mutable wins: int = 0
        for h in hands do
            let p1: string array = unbox<string array> [|_dictGet h ((string ("c0"))); _dictGet h ((string ("c1"))); _dictGet h ((string ("c2"))); _dictGet h ((string ("c3"))); _dictGet h ((string ("c4")))|]
            let p2: string array = unbox<string array> [|_dictGet h ((string ("c5"))); _dictGet h ((string ("c6"))); _dictGet h ((string ("c7"))); _dictGet h ((string ("c8"))); _dictGet h ((string ("c9")))|]
            if (compare_hands (p1) (p2)) = 1 then
                wins <- wins + 1
        __ret <- wins
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%d" (solution()))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
