// Generated 2025-07-26 05:17 +0700

exception Return

let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
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
and floor (x: float) =
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
and absFloat (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and absInt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- (n * 10) + (unbox<int> (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and parseDate (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let y: int = parseIntStr (s.Substring(0, 4 - 0))
        let m: int = parseIntStr (s.Substring(5, 7 - 5))
        let d: int = parseIntStr (s.Substring(8, 10 - 8))
        __ret <- [|y; m; d|]
        raise Return
        __ret
    with
        | Return -> __ret
and leap (y: int) =
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
and daysInMonth (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    try
        let feb: int = if leap y then 29 else 28
        let lengths: int array = [|31; feb; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
        __ret <- lengths.[m - 1]
        raise Return
        __ret
    with
        | Return -> __ret
and addDays (y: int) (m: int) (d: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    let mutable n = n
    try
        let mutable yy: int = y
        let mutable mm: int = m
        let mutable dd: int = d
        if n >= 0 then
            let mutable i: int = 0
            while i < n do
                dd <- dd + 1
                if dd > (unbox<int> (daysInMonth yy mm)) then
                    dd <- 1
                    mm <- mm + 1
                    if mm > 12 then
                        mm <- 1
                        yy <- yy + 1
                i <- i + 1
        else
            let mutable i: int = 0
            while i > n do
                dd <- dd - 1
                if dd < 1 then
                    mm <- mm - 1
                    if mm < 1 then
                        mm <- 12
                        yy <- yy - 1
                    dd <- daysInMonth yy mm
                i <- i - 1
        __ret <- [|yy; mm; dd|]
        raise Return
        __ret
    with
        | Return -> __ret
and pad2 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- if n < 10 then ("0" + (string n)) else (string n)
        raise Return
        __ret
    with
        | Return -> __ret
and dateString (y: int) (m: int) (d: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        __ret <- ((((string y) + "-") + (unbox<string> (pad2 m))) + "-") + (unbox<string> (pad2 d))
        raise Return
        __ret
    with
        | Return -> __ret
and day (y: int) (m: int) (d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        let part1: int = 367 * y
        let part2: int = int ((7 * (int (y + ((m + 9) / 12)))) / 4)
        let part3: int = int ((275 * m) / 9)
        __ret <- (((part1 - part2) + part3) + d) - 730530
        raise Return
        __ret
    with
        | Return -> __ret
and biorhythms (birth: string) (target: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable birth = birth
    let mutable target = target
    try
        let bparts: int array = parseDate birth
        let by: int = bparts.[0]
        let bm: int = bparts.[1]
        let bd: int = bparts.[2]
        let tparts: int array = parseDate target
        let ty: int = tparts.[0]
        let tm: int = tparts.[1]
        let td: int = tparts.[2]
        let diff: int = absInt (unbox<int> ((day ty tm td) - (day by bm bd)))
        printfn "%s" ((("Born " + birth) + ", Target ") + target)
        printfn "%s" ("Day " + (string diff))
        let cycles: string array = [|"Physical day "; "Emotional day"; "Mental day   "|]
        let lengths: int array = [|23; 28; 33|]
        let quadrants: string array array = [|[|"up and rising"; "peak"|]; [|"up but falling"; "transition"|]; [|"down and falling"; "valley"|]; [|"down but rising"; "transition"|]|]
        let mutable i: int = 0
        while i < 3 do
            let length: int = lengths.[i]
            let cycle: string = cycles.[i]
            let position: int = ((diff % length + length) % length)
            let quadrant: int = (position * 4) / length
            let mutable percent: float = sinApprox (((2.0 * PI) * (float position)) / (float length))
            percent <- (unbox<float> (floor (percent * 1000.0))) / 10.0
            let mutable description: string = ""
            if percent > 95.0 then
                description <- " peak"
            else
                if percent < (-95.0) then
                    description <- " valley"
                else
                    if (unbox<float> (absFloat percent)) < 5.0 then
                        description <- " critical transition"
                    else
                        let daysToAdd: int = (((quadrant + 1) * length) / 4) - position
                        let res: int array = addDays ty tm td daysToAdd
                        let ny: int = res.[0]
                        let nm: int = res.[1]
                        let nd: int = res.[2]
                        let transition: string = dateString ny nm nd
                        let trend = (quadrants.[quadrant]).[0]
                        let next = (quadrants.[quadrant]).[1]
                        let mutable pct: string = string percent
                        if not (pct.Contains(".")) then
                            pct <- pct + ".0"
                        description <- (((((((" " + pct) + "% (") + (unbox<string> trend)) + ", next ") + (unbox<string> next)) + " ") + transition) + ")"
            let mutable posStr: string = string position
            if position < 10 then
                posStr <- " " + posStr
            printfn "%s" (((cycle + posStr) + " : ") + description)
            i <- i + 1
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let pairs: string array array = [|[|"1943-03-09"; "1972-07-11"|]; [|"1809-01-12"; "1863-11-19"|]; [|"1809-02-12"; "1863-11-19"|]|]
        let mutable idx: int = 0
        while idx < (unbox<int> (Array.length pairs)) do
            let p: string array = pairs.[idx]
            biorhythms (unbox<string> (p.[0])) (unbox<string> (p.[1]))
            idx <- idx + 1
        __ret
    with
        | Return -> __ret
main()
