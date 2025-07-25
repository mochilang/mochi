// Generated 2025-07-26 04:38 +0700

exception Return

let rec padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and padRight (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable out: string = s
        let mutable i: int = String.length s
        while i < w do
            out <- out + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and format2 (f: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        let mutable s: string = string f
        let idx: int = indexOf s "."
        if idx < 0 then
            s <- s + ".00"
        else
            let mutable need: int = idx + 3
            if (String.length s) > need then
                s <- s.Substring(0, need - 0)
            else
                while (String.length s) < need do
                    s <- s + "0"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and cpx (h: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    try
        let mutable x: int = int ((h / 11.25) + 0.5)
        x <- ((x % 32 + 32) % 32)
        if x < 0 then
            x <- x + 32
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let compassPoint: string array = [|"North"; "North by east"; "North-northeast"; "Northeast by north"; "Northeast"; "Northeast by east"; "East-northeast"; "East by north"; "East"; "East by south"; "East-southeast"; "Southeast by east"; "Southeast"; "Southeast by south"; "South-southeast"; "South by east"; "South"; "South by west"; "South-southwest"; "Southwest by south"; "Southwest"; "Southwest by west"; "West-southwest"; "West by south"; "West"; "West by north"; "West-northwest"; "Northwest by west"; "Northwest"; "Northwest by north"; "North-northwest"; "North by west"|]
let rec degrees2compasspoint (h: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable h = h
    try
        __ret <- compassPoint.[cpx h]
        raise Return
        __ret
    with
        | Return -> __ret
let headings: float array = [|0.0; 16.87; 16.88; 33.75; 50.62; 50.63; 67.5; 84.37; 84.38; 101.25; 118.12; 118.13; 135.0; 151.87; 151.88; 168.75; 185.62; 185.63; 202.5; 219.37; 219.38; 236.25; 253.12; 253.13; 270.0; 286.87; 286.88; 303.75; 320.62; 320.63; 337.5; 354.37; 354.38|]
printfn "%s" "Index  Compass point         Degree"
let mutable i: int = 0
while i < (int (Array.length headings)) do
    let h: float = headings.[i]
    let idx: int = (((i % 32 + 32) % 32)) + 1
    let cp: string = degrees2compasspoint h
    printfn "%s" ((((((unbox<string> (padLeft (string idx) 4)) + "   ") + (unbox<string> (padRight cp 19))) + " ") + (unbox<string> (format2 h))) + "°")
    i <- i + 1
