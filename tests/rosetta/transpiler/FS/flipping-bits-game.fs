// Generated 2025-07-30 21:05 +0700

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
let rec randInt (seed: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable seed = seed
    let mutable n = n
    try
        let next: int = ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- unbox<int array> [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
and newBoard (n: int) (seed: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable n = n
    let mutable seed = seed
    try
        let mutable board: int array array = [||]
        let mutable s: int = seed
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                let mutable r: int array = randInt s 2
                s <- r.[0]
                row <- Array.append row [|r.[1]|]
                j <- j + 1
            board <- Array.append board [|row|]
            i <- i + 1
        __ret <- [|box board; box s|]
        raise Return
        __ret
    with
        | Return -> __ret
and copyBoard (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable b = b
    try
        let mutable nb: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length b) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (b.[i])) do
                row <- Array.append row [|(b.[i]).[j]|]
                j <- j + 1
            nb <- Array.append nb [|row|]
            i <- i + 1
        __ret <- nb
        raise Return
        __ret
    with
        | Return -> __ret
and flipRow (b: int array array) (r: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable b = b
    let mutable r = r
    try
        let mutable j: int = 0
        while j < (Seq.length (b.[r])) do
            (b.[r]).[j] <- 1 - ((b.[r]).[j])
            j <- j + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and flipCol (b: int array array) (c: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable b = b
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (Seq.length b) do
            (b.[i]).[c] <- 1 - ((b.[i]).[c])
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and boardsEqual (a: int array array) (b: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        while i < (Seq.length a) do
            let mutable j: int = 0
            while j < (Seq.length (a.[i])) do
                if ((a.[i]).[j]) <> ((b.[i]).[j]) then
                    __ret <- false
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and shuffleBoard (b: int array array) (seed: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable b = b
    let mutable seed = seed
    try
        let mutable s: int = seed
        let mutable n: int = Seq.length b
        let mutable k: int = 0
        while k < (2 * n) do
            let mutable r: int array = randInt s n
            s <- r.[0]
            let idx: int = int (r.[1])
            if (((k % 2 + 2) % 2)) = 0 then
                b <- flipRow b idx
            else
                b <- flipCol b idx
            k <- k + 1
        __ret <- [|box b; box s|]
        raise Return
        __ret
    with
        | Return -> __ret
and solve (board: int array array) (target: int array array) =
    let mutable __ret : Map<string, int array> = Unchecked.defaultof<Map<string, int array>>
    let mutable board = board
    let mutable target = target
    try
        let n: int = Seq.length board
        let mutable row: int array = [||]
        let mutable col: int array = [||]
        let mutable i: int = 0
        while i < n do
            let diff: int = if ((board.[i]).[0]) <> ((target.[i]).[0]) then 1 else 0
            row <- Array.append row [|diff|]
            i <- i + 1
        let mutable j: int = 0
        while j < n do
            let diff: int = if ((board.[0]).[j]) <> ((target.[0]).[j]) then 1 else 0
            let ``val``: int = (((diff + (row.[0])) % 2 + 2) % 2)
            col <- Array.append col [|``val``|]
            j <- j + 1
        __ret <- Map.ofList [("row", row); ("col", col)]
        raise Return
        __ret
    with
        | Return -> __ret
and applySolution (b: int array array) (sol: Map<string, int array>) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable b = b
    let mutable sol = sol
    try
        let mutable board: int array array = b
        let mutable moves: int = 0
        let mutable i: int = 0
        while i < (Seq.length (sol.["row"] |> unbox<int array>)) do
            if (int ((sol.["row"] |> unbox<int array>).[i])) = 1 then
                board <- flipRow board i
                moves <- moves + 1
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (sol.["col"] |> unbox<int array>)) do
            if (int ((sol.["col"] |> unbox<int array>).[j])) = 1 then
                board <- flipCol board j
                moves <- moves + 1
            j <- j + 1
        __ret <- [|box board; box moves|]
        raise Return
        __ret
    with
        | Return -> __ret
and printBoard (b: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        let mutable i: int = 0
        while i < (Seq.length b) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (b.[i])) do
                line <- line + (string ((b.[i]).[j]))
                if j < ((Seq.length (b.[i])) - 1) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let n: int = 3
        let mutable seed: int = 1
        let mutable res: obj array = newBoard n seed
        let mutable target: int array array = (match (res.[0]) with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
        seed <- unbox<int> (res.[1])
        let mutable board: int array array = copyBoard target
        try
            while true do
                try
                    let mutable sres: obj array = shuffleBoard (copyBoard board) seed
                    board <- (match (sres.[0]) with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
                    seed <- unbox<int> (sres.[1])
                    if not (boardsEqual board target) then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" "Target:"
        printBoard target
        printfn "%s" "Board:"
        printBoard board
        let sol: Map<string, int array> = solve board target
        let mutable ares: obj array = applySolution board sol
        board <- (match (ares.[0]) with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
        let moves: int = unbox<int> (ares.[1])
        printfn "%s" "Solved:"
        printBoard board
        printfn "%s" ("Moves: " + (string moves))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
