// Generated 2025-07-31 00:10 +0700

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
let dayNames: string array = [|"Sweetmorn"; "Boomtime"; "Pungenday"; "Prickle-Prickle"; "Setting Orange"|]
let seasons: string array = [|"Chaos"; "Discord"; "Confusion"; "Bureaucracy"; "The Aftermath"|]
let holydays: string array array = [|[|"Mungday"; "Chaoflux"|]; [|"Mojoday"; "Discoflux"|]; [|"Syaday"; "Confuflux"|]; [|"Zaraday"; "Bureflux"|]; [|"Maladay"; "Afflux"|]|]
let rec isLeap (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable y = y
    try
        if (((y % 400 + 400) % 400)) = 0 then
            __ret <- true
            raise Return
        if (((y % 100 + 100) % 100)) = 0 then
            __ret <- false
            raise Return
        __ret <- (((y % 4 + 4) % 4)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let daysBefore: int array = [|0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334|]
let rec dayOfYear (y: int) (m: int) (d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        let mutable doy: int = (daysBefore.[m - 1]) + d
        if (m > 2) && (unbox<bool> (isLeap y)) then
            doy <- doy + 1
        __ret <- doy
        raise Return
        __ret
    with
        | Return -> __ret
and ordinal (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable suff: string = "th"
        let mod100: int = ((n % 100 + 100) % 100)
        if (mod100 < 11) || (mod100 > 13) then
            let r: int = ((n % 10 + 10) % 10)
            if r = 1 then
                suff <- "st"
            else
                if r = 2 then
                    suff <- "nd"
                else
                    if r = 3 then
                        suff <- "rd"
        __ret <- (string n) + suff
        raise Return
        __ret
    with
        | Return -> __ret
and discordian (y: int) (m: int) (d: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        if ((unbox<bool> (isLeap y)) && (m = 2)) && (d = 29) then
            __ret <- "St. Tib's Day, YOLD " + (string (y + 1166))
            raise Return
        let mutable doy: int = dayOfYear y m d
        if (unbox<bool> (isLeap y)) && (doy > 60) then
            doy <- doy - 1
        let mutable idx: int = doy - 1
        let season: int = idx / 73
        let day: int = ((idx % 73 + 73) % 73)
        let mutable res: string = ((((((dayNames.[((idx % 5 + 5) % 5)]) + ", the ") + (unbox<string> (ordinal (day + 1)))) + " day of ") + (seasons.[season])) + " in the YOLD ") + (string (y + 1166))
        if day = 4 then
            res <- ((res + ". Celebrate ") + ((holydays.[season]).[0])) + "!"
        if day = 49 then
            res <- ((res + ". Celebrate ") + ((holydays.[season]).[1])) + "!"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let dates: int array array = [|[|2010; 7; 22|]; [|2012; 2; 28|]; [|2012; 2; 29|]; [|2012; 3; 1|]; [|2012; 12; 31|]; [|2013; 1; 1|]; [|2100; 12; 31|]; [|2015; 10; 19|]; [|2010; 1; 5|]; [|2011; 5; 3|]; [|2000; 3; 13|]|]
        let mutable i: int = 0
        while i < (Seq.length dates) do
            let dt: int array = dates.[i]
            printfn "%s" (discordian (dt.[0]) (dt.[1]) (dt.[2]))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
