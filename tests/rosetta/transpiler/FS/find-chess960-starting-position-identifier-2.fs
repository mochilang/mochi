// Generated 2025-07-30 21:05 +0700

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

let mutable g2lMap: Map<string, string> = Map.ofList [("♜", "R"); ("♞", "N"); ("♝", "B"); ("♛", "Q"); ("♚", "K"); ("♖", "R"); ("♘", "N"); ("♗", "B"); ("♕", "Q"); ("♔", "K")]
let mutable names: Map<string, string> = Map.ofList [("R", "rook"); ("N", "knight"); ("B", "bishop"); ("Q", "queen"); ("K", "king")]
let mutable ntable: Map<string, int> = Map.ofList [("01", 0); ("02", 1); ("03", 2); ("04", 3); ("12", 4); ("13", 5); ("14", 6); ("23", 7); ("24", 8); ("34", 9)]
let rec indexOf (s: string) (sub: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    try
        let mutable i: int = 0
        while i <= ((String.length s) - (String.length sub)) do
            if (_substring s i (i + (String.length sub))) = sub then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and removeChar (s: string) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let c: string = _substring s i (i + 1)
            if c <> ch then
                res <- res + c
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and g2l (pieces: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable pieces = pieces
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length pieces) do
            let ch: string = _substring pieces i (i + 1)
            res <- res + (unbox<string> (g2lMap.[ch] |> unbox<string>))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and countChar (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable c: int = 0
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                c <- c + 1
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
and spid (pieces: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable pieces = pieces
    try
        pieces <- g2l pieces
        if (String.length pieces) <> 8 then
            __ret <- -1
            raise Return
        for one in [|"K"; "Q"|] do
            if (int (countChar pieces (unbox<string> one))) <> 1 then
                __ret <- -1
                raise Return
        for two in [|"R"; "N"; "B"|] do
            if (int (countChar pieces (unbox<string> two))) <> 2 then
                __ret <- -1
                raise Return
        let r1: int = pieces.IndexOf("R")
        let r2 = (int ((int ((_substring pieces (r1 + 1) (String.length pieces)).IndexOf("R"))) + r1)) + 1
        let k: int = pieces.IndexOf("K")
        if (k < r1) || (k > (int r2)) then
            __ret <- -1
            raise Return
        let b1: int = pieces.IndexOf("B")
        let b2 = (int ((int ((_substring pieces (b1 + 1) (String.length pieces)).IndexOf("B"))) + b1)) + 1
        if (int (((((int b2) - b1) % 2 + 2) % 2))) = 0 then
            __ret <- -1
            raise Return
        let mutable piecesN: string = removeChar (removeChar pieces "Q") "B"
        let n1: int = piecesN.IndexOf("N")
        let n2 = (int ((int ((_substring piecesN (n1 + 1) (String.length piecesN)).IndexOf("N"))) + n1)) + 1
        let N: int = ntable.[((string n1) + (string n2))] |> unbox<int>
        let mutable piecesQ: string = removeChar pieces "B"
        let Q: int = piecesQ.IndexOf("Q")
        let mutable D: int = "0246".IndexOf(string b1)
        let mutable L: int = "1357".IndexOf(string b2)
        if D = (0 - 1) then
            D <- int ("0246".IndexOf(string b2))
            L <- int ("1357".IndexOf(string b1))
        __ret <- (((96 * N) + (16 * Q)) + (4 * D)) + L
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for pieces in [|"♕♘♖♗♗♘♔♖"; "♖♘♗♕♔♗♘♖"; "♖♕♘♗♗♔♖♘"; "♖♘♕♗♗♔♖♘"|] do
            printfn "%s" (((((unbox<string> pieces) + " or ") + (unbox<string> (g2l (unbox<string> pieces)))) + " has SP-ID of ") + (string (spid (unbox<string> pieces))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
