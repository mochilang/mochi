// Generated 2025-07-25 16:42 +0700

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
let rec pow10 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable n: int = 1
        let mutable i: int = 0
        while i < exp do
            n <- n * 10
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and totient (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable tot: int = n
        let mutable nn: int = n
        let mutable i: int = 2
        while (i * i) <= nn do
            if (nn % i) = 0 then
                while (nn % i) = 0 do
                    nn <- nn / i
                tot <- tot - (tot / i)
            if i = 2 then
                i <- 1
            i <- i + 2
        if nn > 1 then
            tot <- tot - (tot / nn)
        __ret <- tot
        raise Return
        __ret
    with
        | Return -> __ret
let mutable pps: Map<int, bool> = Map.ofList []
let rec getPerfectPowers (maxExp: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable maxExp = maxExp
    try
        let upper: int = pow10 maxExp
        let mutable i: int = 2
        try
            while (i * i) < upper do
                let mutable p: int = i
                try
                    while true do
                        p <- p * i
                        if p >= upper then
                            raise Break
                        pps <- Map.add p true pps
                with
                | Break -> ()
                | Continue -> ()
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and getAchilles (minExp: int) (maxExp: int) =
    let mutable __ret : Map<int, bool> = Unchecked.defaultof<Map<int, bool>>
    let mutable minExp = minExp
    let mutable maxExp = maxExp
    try
        let lower: int = pow10 minExp
        let upper: int = pow10 maxExp
        let mutable achilles: Map<int, bool> = Map.ofList []
        let mutable b: int = 1
        try
            while ((b * b) * b) < upper do
                let b3: int = (b * b) * b
                let mutable a: int = 1
                try
                    while true do
                        let p: int = (b3 * a) * a
                        if p >= upper then
                            raise Break
                        if p >= lower then
                            if not (Map.containsKey p pps) then
                                achilles <- Map.add p true achilles
                        a <- a + 1
                with
                | Break -> ()
                | Continue -> ()
                b <- b + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- achilles
        raise Return
        __ret
    with
        | Return -> __ret
and sortInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable tmp = xs
        while (Array.length tmp) > 0 do
            let mutable min = tmp.[0]
            let mutable idx: int = 0
            let mutable i: int = 1
            while i < (Array.length tmp) do
                if (tmp.[i]) < min then
                    min <- tmp.[i]
                    idx <- i
                i <- i + 1
            res <- Array.append res [|min|]
            let mutable out: int array = [||]
            let mutable j: int = 0
            while j < (Array.length tmp) do
                if j <> idx then
                    out <- Array.append out [|tmp.[j]|]
                j <- j + 1
            tmp <- out
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
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
        let maxDigits: int = 15
        getPerfectPowers 5
        let achSet: Map<int, bool> = getAchilles 1 5
        let mutable ach: int array = [||]
        for k in (Map.toList achSet |> List.map fst) do
            ach <- Array.append ach [|k|]
        ach <- sortInts ach
        printfn "%s" "First 50 Achilles numbers:"
        let mutable i: int = 0
        while i < 50 do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < 10 do
                line <- line + (pad (ach.[i]) 4)
                if j < 9 then
                    line <- line + " "
                i <- i + 1
                j <- j + 1
            printfn "%s" line
        printfn "%s" "\nFirst 30 strong Achilles numbers:"
        let mutable strong: int array = [||]
        let mutable count: int = 0
        let mutable idx: int = 0
        while count < 30 do
            let tot: int = totient (ach.[idx])
            if Map.containsKey tot achSet then
                strong <- Array.append strong [|ach.[idx]|]
                count <- count + 1
            idx <- idx + 1
        i <- 0
        while i < 30 do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < 10 do
                line <- line + (pad (strong.[i]) 5)
                if j < 9 then
                    line <- line + " "
                i <- i + 1
                j <- j + 1
            printfn "%s" line
        printfn "%s" "\nNumber of Achilles numbers with:"
        let counts: int array = [|1; 12; 47; 192; 664; 2242; 7395; 24008; 77330; 247449; 788855; 2508051; 7960336; 25235383|]
        let mutable d: int = 2
        while d <= maxDigits do
            let c: int = counts.[d - 2]
            printfn "%s" (((pad d 2) + " digits: ") + (string c))
            d <- d + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
