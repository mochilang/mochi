// Generated 2025-07-28 10:03 +0700

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
let rec bitAt (x: int) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable idx = idx
    try
        let mutable v: int = x
        let mutable i: int = 0
        while i < idx do
            v <- int (v / 2)
            i <- i + 1
        __ret <- ((v % 2 + 2) % 2)
        raise Return
        __ret
    with
        | Return -> __ret
and outputState (state: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable state = state
    try
        let mutable line: string = ""
        let mutable i: int = 0
        while i < (String.length state) do
            if (state.Substring(i, (i + 1) - i)) = "1" then
                line <- line + "#"
            else
                line <- line + " "
            i <- i + 1
        printfn "%s" line
        __ret
    with
        | Return -> __ret
and step (state: string) (r: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable state = state
    let mutable r = r
    try
        let cells: int = String.length state
        let mutable out: string = ""
        let mutable i: int = 0
        while i < cells do
            let l: string = state.Substring(((((i - 1) + cells) % cells + cells) % cells), ((((((i - 1) + cells) % cells + cells) % cells)) + 1) - (((((i - 1) + cells) % cells + cells) % cells)))
            let c: string = state.Substring(i, (i + 1) - i)
            let rt: string = state.Substring((((i + 1) % cells + cells) % cells), (((((i + 1) % cells + cells) % cells)) + 1) - ((((i + 1) % cells + cells) % cells)))
            let mutable idx: int = 0
            if l = "1" then
                idx <- idx + 4
            if c = "1" then
                idx <- idx + 2
            if rt = "1" then
                idx <- idx + 1
            if (int (bitAt r idx)) = 1 then
                out <- out + "1"
            else
                out <- out + "0"
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and elem (r: int) (cells: int) (generations: int) (state: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable r = r
    let mutable cells = cells
    let mutable generations = generations
    let mutable state = state
    try
        outputState state
        let mutable g: int = 0
        let mutable s: string = state
        while g < generations do
            s <- step s r
            outputState s
            g <- g + 1
        __ret
    with
        | Return -> __ret
and randInit (cells: int) (seed: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cells = cells
    let mutable seed = seed
    try
        let mutable s: string = ""
        let mutable ``val``: int = seed
        let mutable i: int = 0
        while i < cells do
            ``val`` <- ((((``val`` * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
            if (((``val`` % 2 + 2) % 2)) = 0 then
                s <- s + "0"
            else
                s <- s + "1"
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and singleInit (cells: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cells = cells
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < cells do
            if i = (cells / 2) then
                s <- s + "1"
            else
                s <- s + "0"
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let cells: int = 20
        let generations: int = 9
        printfn "%s" "Single 1, rule 90:"
        let mutable state: string = singleInit cells
        elem 90 cells generations state
        printfn "%s" "Random intial state, rule 30:"
        state <- randInit cells 3
        elem 30 cells generations state
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
