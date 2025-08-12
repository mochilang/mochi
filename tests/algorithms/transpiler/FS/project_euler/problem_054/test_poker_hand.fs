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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
type Hand = {
    mutable _rank: int
    mutable _values: int array
}
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = Array.empty<string>
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (((String.length (sep)) > 0) && ((i + (String.length (sep))) <= (String.length (s)))) && ((_substring s i (i + (String.length (sep)))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length (sep))
            else
                cur <- cur + (_substring s (i) (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and card_value (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "A" then
            __ret <- 14
            raise Return
        else
            if ch = "K" then
                __ret <- 13
                raise Return
            else
                if ch = "Q" then
                    __ret <- 12
                    raise Return
                else
                    if ch = "J" then
                        __ret <- 11
                        raise Return
                    else
                        if ch = "T" then
                            __ret <- 10
                            raise Return
                        else
                            if ch = "9" then
                                __ret <- 9
                                raise Return
                            else
                                if ch = "8" then
                                    __ret <- 8
                                    raise Return
                                else
                                    if ch = "7" then
                                        __ret <- 7
                                        raise Return
                                    else
                                        if ch = "6" then
                                            __ret <- 6
                                            raise Return
                                        else
                                            if ch = "5" then
                                                __ret <- 5
                                                raise Return
                                            else
                                                if ch = "4" then
                                                    __ret <- 4
                                                    raise Return
                                                else
                                                    if ch = "3" then
                                                        __ret <- 3
                                                        raise Return
                                                    else
                                                        __ret <- 2
                                                        raise Return
        __ret
    with
        | Return -> __ret
and parse_hand (hand: string) =
    let mutable __ret : Hand = Unchecked.defaultof<Hand>
    let mutable hand = hand
    try
        let mutable counts: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= 14 do
            counts <- Array.append counts [|0|]
            i <- i + 1
        let mutable suits: string array = Array.empty<string>
        for card in Seq.map string (split (hand) (" ")) do
            let mutable v: int = card_value (_substring card (0) (1))
            counts.[v] <- (_idx counts (int v)) + 1
            suits <- Array.append suits [|(_substring card (1) (2))|]
        let mutable vals: int array = Array.empty<int>
        let mutable v: int = 14
        while v >= 2 do
            let mutable c: int = _idx counts (int v)
            let mutable k: int = 0
            while k < c do
                vals <- Array.append vals [|v|]
                k <- k + 1
            v <- v - 1
        let mutable is_straight: bool = false
        if ((((((Seq.length (vals)) = 5) && ((_idx vals (int 0)) = 14)) && ((_idx vals (int 1)) = 5)) && ((_idx vals (int 2)) = 4)) && ((_idx vals (int 3)) = 3)) && ((_idx vals (int 4)) = 2) then
            is_straight <- true
            vals.[0] <- 5
            vals.[1] <- 4
            vals.[2] <- 3
            vals.[3] <- 2
            vals.[4] <- 14
        else
            is_straight <- true
            let mutable j: int = 0
            while j < 4 do
                if ((_idx vals (int j)) - (_idx vals (int (j + 1)))) <> 1 then
                    is_straight <- false
                j <- j + 1
        let mutable is_flush: bool = true
        let mutable s0: string = _idx suits (int 0)
        let mutable t: int = 1
        while t < (Seq.length (suits)) do
            if (_idx suits (int t)) <> s0 then
                is_flush <- false
            t <- t + 1
        let mutable four_val: int = 0
        let mutable three_val: int = 0
        let mutable pair_vals: int array = Array.empty<int>
        v <- 14
        while v >= 2 do
            if (_idx counts (int v)) = 4 then
                four_val <- v
            else
                if (_idx counts (int v)) = 3 then
                    three_val <- v
                else
                    if (_idx counts (int v)) = 2 then
                        pair_vals <- Array.append pair_vals [|v|]
            v <- v - 1
        let mutable _rank: int = 1
        if ((is_flush && is_straight) && ((_idx vals (int 0)) = 14)) && ((_idx vals (int 4)) = 10) then
            _rank <- 10
        else
            if is_flush && is_straight then
                _rank <- 9
            else
                if four_val <> 0 then
                    _rank <- 8
                else
                    if (three_val <> 0) && ((Seq.length (pair_vals)) = 1) then
                        _rank <- 7
                    else
                        if is_flush then
                            _rank <- 6
                        else
                            if is_straight then
                                _rank <- 5
                            else
                                if three_val <> 0 then
                                    _rank <- 4
                                else
                                    if (Seq.length (pair_vals)) = 2 then
                                        _rank <- 3
                                    else
                                        if (Seq.length (pair_vals)) = 1 then
                                            _rank <- 2
                                        else
                                            _rank <- 1
        __ret <- { _rank = _rank; _values = vals }
        raise Return
        __ret
    with
        | Return -> __ret
and compare (a: Hand) (b: Hand) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        if (a._rank) > (b._rank) then
            __ret <- "Win"
            raise Return
        if (a._rank) < (b._rank) then
            __ret <- "Loss"
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a._values)) do
            if (_idx (a._values) (int i)) > (_idx (b._values) (int i)) then
                __ret <- "Win"
                raise Return
            if (_idx (a._values) (int i)) < (_idx (b._values) (int i)) then
                __ret <- "Loss"
                raise Return
            i <- i + 1
        __ret <- "Tie"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tests: string array array = [|[|"2H 3H 4H 5H 6H"; "KS AS TS QS JS"; "Loss"|]; [|"2H 3H 4H 5H 6H"; "AS AD AC AH JD"; "Win"|]; [|"AS AH 2H AD AC"; "JS JD JC JH 3D"; "Win"|]; [|"2S AH 2H AS AC"; "JS JD JC JH AD"; "Loss"|]; [|"2S AH 2H AS AC"; "2H 3H 5H 6H 7H"; "Win"|]|]
        for t in tests do
            let res: string = compare (parse_hand (_idx t (int 0))) (parse_hand (_idx t (int 1)))
            ignore (printfn "%s" ((res + " expected ") + (_idx t (int 2))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
