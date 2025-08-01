// Generated 2025-08-01 12:46 +0000

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let gregorianStr: string array = [|"January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]
let gregorian: int array = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
let republicanStr: string array = [|"Vendemiaire"; "Brumaire"; "Frimaire"; "Nivose"; "Pluviose"; "Ventose"; "Germinal"; "Floreal"; "Prairial"; "Messidor"; "Thermidor"; "Fructidor"|]
let sansculotidesStr: string array = [|"Fete de la vertu"; "Fete du genie"; "Fete du travail"; "Fete de l'opinion"; "Fete des recompenses"; "Fete de la Revolution"|]
let rec greLeap (year: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable year = year
    try
        let a: int = int (((year % 4 + 4) % 4))
        let b: int = int (((year % 100 + 100) % 100))
        let c: int = int (((year % 400 + 400) % 400))
        __ret <- (a = 0) && ((b <> 0) || (c = 0))
        raise Return
        __ret
    with
        | Return -> __ret
and repLeap (year: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable year = year
    try
        let a: int = int ((((year + 1) % 4 + 4) % 4))
        let b: int = int ((((year + 1) % 100 + 100) % 100))
        let c: int = int ((((year + 1) % 400 + 400) % 400))
        __ret <- (a = 0) && ((b <> 0) || (c = 0))
        raise Return
        __ret
    with
        | Return -> __ret
and greToDay (d: int) (m: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable d = d
    let mutable m = m
    let mutable y = y
    try
        let mutable yy: int = y
        let mutable mm: int = m
        if mm < 3 then
            yy <- yy - 1
            mm <- mm + 12
        __ret <- ((((((yy * 36525) / 100) - (yy / 100)) + (yy / 400)) + ((306 * (mm + 1)) / 10)) + d) - 654842
        raise Return
        __ret
    with
        | Return -> __ret
and repToDay (d: int) (m: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable d = d
    let mutable m = m
    let mutable y = y
    try
        let mutable dd: int = d
        let mutable mm: int = m
        if mm = 13 then
            mm <- mm - 1
            dd <- dd + 30
        if repLeap y then
            dd <- dd - 1
        __ret <- ((((((365 * y) + ((y + 1) / 4)) - ((y + 1) / 100)) + ((y + 1) / 400)) + (30 * mm)) + dd) - 395
        raise Return
        __ret
    with
        | Return -> __ret
and dayToGre (day: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable day = day
    try
        let mutable y: int = (day * 100) / 36525
        let mutable d: int = (day - ((y * 36525) / 100)) + 21
        y <- y + 1792
        d <- ((d + (y / 100)) - (y / 400)) - 13
        let mutable m: int = 8
        while d > (gregorian.[m]) do
            d <- d - (gregorian.[m])
            m <- m + 1
            if m = 12 then
                m <- 0
                y <- y + 1
                if greLeap y then
                    gregorian.[1] <- 29
                else
                    gregorian.[1] <- 28
        m <- m + 1
        __ret <- unbox<int array> [|d; m; y|]
        raise Return
        __ret
    with
        | Return -> __ret
and dayToRep (day: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable day = day
    try
        let mutable y: int = ((day - 1) * 100) / 36525
        if repLeap y then
            y <- y - 1
        let mutable d: int = (((day - (((y + 1) * 36525) / 100)) + 365) + ((y + 1) / 100)) - ((y + 1) / 400)
        y <- y + 1
        let mutable m: int = 1
        let mutable sc: int = 5
        if repLeap y then
            sc <- 6
        while d > 30 do
            d <- d - 30
            m <- m + 1
            if m = 13 then
                if d > sc then
                    d <- d - sc
                    m <- 1
                    y <- y + 1
                    sc <- 5
                    if repLeap y then
                        sc <- 6
        __ret <- unbox<int array> [|d; m; y|]
        raise Return
        __ret
    with
        | Return -> __ret
and formatRep (d: int) (m: int) (y: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    let mutable m = m
    let mutable y = y
    try
        __ret <- if m = 13 then (((sansculotidesStr.[d - 1]) + " ") + (string y)) else (((((string d) + " ") + (republicanStr.[m - 1])) + " ") + (string y))
        raise Return
        __ret
    with
        | Return -> __ret
and formatGre (d: int) (m: int) (y: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    let mutable m = m
    let mutable y = y
    try
        __ret <- ((((string d) + " ") + (gregorianStr.[m - 1])) + " ") + (string y)
        raise Return
        __ret
    with
        | Return -> __ret
let rep: int array = dayToRep (greToDay 20 5 1795)
printfn "%s" (formatRep (rep.[0]) (rep.[1]) (rep.[2]))
let gre: int array = dayToGre (repToDay 1 9 3)
printfn "%s" (formatGre (gre.[0]) (gre.[1]) (gre.[2]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
