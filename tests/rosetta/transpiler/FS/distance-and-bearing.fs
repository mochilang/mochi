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
type Airport = {
    name: string
    country: string
    icao: string
    lat: float
    lon: float
}
let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 8 do
            let denom: float = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and cosApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n <= 8 do
            let denom: float = float (((2 * n) - 1) * (2 * n))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and atanApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x > 1.0 then
            __ret <- (PI / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        if x < (-1.0) then
            __ret <- ((-PI) / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        __ret <- x / (1.0 + ((0.28 * x) * x))
        raise Return
        __ret
    with
        | Return -> __ret
and atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x > 0.0 then
            let r: float = atanApprox (y / x)
            __ret <- r
            raise Return
        if x < 0.0 then
            if y >= 0.0 then
                __ret <- (float (atanApprox (y / x))) + PI
                raise Return
            __ret <- (float (atanApprox (y / x))) - PI
            raise Return
        if y > 0.0 then
            __ret <- PI / 2.0
            raise Return
        if y < 0.0 then
            __ret <- (-PI) / 2.0
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and rad (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (x * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
and deg (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (x * 180.0) / PI
        raise Return
        __ret
    with
        | Return -> __ret
and distance (lat1: float) (lon1: float) (lat2: float) (lon2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable lat1 = lat1
    let mutable lon1 = lon1
    let mutable lat2 = lat2
    let mutable lon2 = lon2
    try
        let phi1: float = rad lat1
        let phi2: float = rad lat2
        let dphi: float = rad (lat2 - lat1)
        let dlambda: float = rad (lon2 - lon1)
        let sdphi: float = sinApprox (dphi / (float 2))
        let sdlambda: float = sinApprox (dlambda / (float 2))
        let a: float = (sdphi * sdphi) + (float ((float ((float ((cosApprox phi1) * (cosApprox phi2))) * sdlambda)) * sdlambda))
        let c: float = 2 * (int (atan2Approx (sqrtApprox a) (sqrtApprox ((float 1) - a))))
        __ret <- (6371.0 / 1.852) * c
        raise Return
        __ret
    with
        | Return -> __ret
and bearing (lat1: float) (lon1: float) (lat2: float) (lon2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable lat1 = lat1
    let mutable lon1 = lon1
    let mutable lat2 = lat2
    let mutable lon2 = lon2
    try
        let phi1: float = rad lat1
        let phi2: float = rad lat2
        let dl: float = rad (lon2 - lon1)
        let y: float = (sinApprox dl) * (cosApprox phi2)
        let x: float = ((cosApprox phi1) * (sinApprox phi2)) - (((sinApprox phi1) * (cosApprox phi2)) * (cosApprox dl))
        let br: float = deg (atan2Approx y x)
        if br < (float 0) then
            br <- br + (float 360)
        __ret <- br
        raise Return
        __ret
    with
        | Return -> __ret
let airports: Airport array = [|{ name = "Koksijde Air Base"; country = "Belgium"; icao = "EBFN"; lat = 51.090301513671875; lon = 2.652780055999756 }; { name = "Ostend-Bruges International Airport"; country = "Belgium"; icao = "EBOS"; lat = 51.198898315399994; lon = 2.8622200489 }; { name = "Kent International Airport"; country = "United Kingdom"; icao = "EGMH"; lat = 51.342201; lon = 1.34611 }; { name = "Calais-Dunkerque Airport"; country = "France"; icao = "LFAC"; lat = 50.962100982666016; lon = 1.954759955406189 }; { name = "Westkapelle heliport"; country = "Belgium"; icao = "EBKW"; lat = 51.32222366333; lon = 3.2930560112 }; { name = "Lympne Airport"; country = "United Kingdom"; icao = "EGMK"; lat = 51.08; lon = 1.013 }; { name = "Ursel Air Base"; country = "Belgium"; icao = "EBUL"; lat = 51.14419937133789; lon = 3.475559949874878 }; { name = "Southend Airport"; country = "United Kingdom"; icao = "EGMC"; lat = 51.5713996887207; lon = 0.6955559849739075 }; { name = "Merville-Calonne Airport"; country = "France"; icao = "LFQT"; lat = 50.61840057373047; lon = 2.642240047454834 }; { name = "Wevelgem Airport"; country = "Belgium"; icao = "EBKT"; lat = 50.817199707; lon = 3.20472002029 }; { name = "Midden-Zeeland Airport"; country = "Netherlands"; icao = "EHMZ"; lat = 51.5121994019; lon = 3.73111009598 }; { name = "Lydd Airport"; country = "United Kingdom"; icao = "EGMD"; lat = 50.95610046386719; lon = 0.9391670227050781 }; { name = "RAF Wattisham"; country = "United Kingdom"; icao = "EGUW"; lat = 52.1273002625; lon = 0.956264019012 }; { name = "Beccles Airport"; country = "United Kingdom"; icao = "EGSM"; lat = 52.435298919699996; lon = 1.6183300018300002 }; { name = "Lille/Marcq-en-Baroeul Airport"; country = "France"; icao = "LFQO"; lat = 50.687198638916016; lon = 3.0755600929260254 }; { name = "Lashenden (Headcorn) Airfield"; country = "United Kingdom"; icao = "EGKH"; lat = 51.156898; lon = 0.641667 }; { name = "Le Touquet-Côte d'Opale Airport"; country = "France"; icao = "LFAT"; lat = 50.517398834228516; lon = 1.6205899715423584 }; { name = "Rochester Airport"; country = "United Kingdom"; icao = "EGTO"; lat = 51.351898193359375; lon = 0.5033329725265503 }; { name = "Lille-Lesquin Airport"; country = "France"; icao = "LFQQ"; lat = 50.563332; lon = 3.086886 }; { name = "Thurrock Airfield"; country = "United Kingdom"; icao = "EGMT"; lat = 51.537505; lon = 0.367634 }|]
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
and pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 n
        __ret <- (float (floor ((x * m) + 0.5))) / m
        raise Return
        __ret
    with
        | Return -> __ret
and sortByDistance (xs: obj array array) =
    let mutable __ret : obj array array = Unchecked.defaultof<obj array array>
    let mutable xs = xs
    try
        let mutable arr: obj array array = xs
        let mutable i: int = 1
        while i < (Seq.length arr) do
            let mutable j: int = i
            while (j > 0) && (((arr.[j - 1]).[0]) > ((arr.[j]).[0])) do
                let tmp: obj array = arr.[j - 1]
                arr.[j - 1] <- arr.[j]
                arr.[j] <- tmp
                j <- j - 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let planeLat: float = 51.514669
        let planeLon: float = 2.198581
        let mutable results: obj array array = [||]
        for ap in airports do
            let d: float = distance planeLat planeLon (ap.lat) (ap.lon)
            let b: float = bearing planeLat planeLon (ap.lat) (ap.lon)
            results <- Array.append results [|[|box d; box b; box ap|]|]
        results <- sortByDistance results
        printfn "%s" "Distance Bearing ICAO Country               Airport"
        printfn "%s" "--------------------------------------------------------------"
        let mutable i: int = 0
        while i < (Seq.length results) do
            let r: obj array = results.[i]
            let ap: obj = r.[2]
            let dist: obj = r.[0]
            let bear: obj = r.[1]
            let line: string = ((((((((string (round (unbox<float> dist) 1)) + "\t") + (string (round (unbox<float> bear) 0))) + "\t") + (unbox<string> (((ap :?> Airport).icao)))) + "\t") + (unbox<string> (((ap :?> Airport).country)))) + " ") + (unbox<string> (((ap :?> Airport).name)))
            printfn "%s" line
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
