// Generated 2025-07-31 00:10 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let months: Map<string, int> = Map.ofList [("January", 1); ("February", 2); ("March", 3); ("April", 4); ("May", 5); ("June", 6); ("July", 7); ("August", 8); ("September", 9); ("October", 10); ("November", 11); ("December", 12)]
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
and daysInMonth (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    try
        let feb: int = if isLeap y then 29 else 28
        let lengths: int array = [|31; feb; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
        __ret <- lengths.[m - 1]
        raise Return
        __ret
    with
        | Return -> __ret
and daysBeforeYear (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    try
        let mutable days: int = 0
        let mutable yy: int = 1970
        while yy < y do
            days <- days + 365
            if isLeap yy then
                days <- days + 1
            yy <- yy + 1
        __ret <- days
        raise Return
        __ret
    with
        | Return -> __ret
and daysBeforeMonth (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    try
        let mutable days: int = 0
        let mutable mm: int = 1
        while mm < m do
            days <- days + (int (daysInMonth y mm))
            mm <- mm + 1
        __ret <- days
        raise Return
        __ret
    with
        | Return -> __ret
and epochSeconds (y: int) (m: int) (d: int) (h: int) (mi: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    let mutable h = h
    let mutable mi = mi
    try
        let mutable days: int = (int ((daysBeforeYear y) + (daysBeforeMonth y m))) + (d - 1)
        __ret <- ((days * 86400) + (h * 3600)) + (mi * 60)
        raise Return
        __ret
    with
        | Return -> __ret
and fromEpoch (sec: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable sec = sec
    try
        let mutable days: int = sec / 86400
        let mutable rem: int = ((sec % 86400 + 86400) % 86400)
        let mutable y: int = 1970
        try
            while true do
                try
                    let dy: int = if isLeap y then 366 else 365
                    if days >= dy then
                        days <- days - dy
                        y <- y + 1
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable m: int = 1
        try
            while true do
                try
                    let dim: int = daysInMonth y m
                    if days >= dim then
                        days <- days - dim
                        m <- m + 1
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let d: int = days + 1
        let h: int = rem / 3600
        let mi: int = (((rem % 3600 + 3600) % 3600)) / 60
        __ret <- unbox<int array> [|y; m; d; h; mi|]
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
and absInt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and formatDate (parts: int array) (offset: int) (abbr: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable parts = parts
    let mutable offset = offset
    let mutable abbr = abbr
    try
        let mutable y: int = parts.[0]
        let mutable m: int = parts.[1]
        let d: int = parts.[2]
        let h: int = parts.[3]
        let mi: int = parts.[4]
        let mutable sign: string = "+"
        if offset < 0 then
            sign <- "-"
        let off: int = (int (absInt offset)) / 60
        let offh: string = pad2 (off / 60)
        let offm: string = pad2 (((off % 60 + 60) % 60))
        __ret <- ((((((((((((((string y) + "-") + (unbox<string> (pad2 m))) + "-") + (unbox<string> (pad2 d))) + " ") + (unbox<string> (pad2 h))) + ":") + (unbox<string> (pad2 mi))) + ":00 ") + sign) + offh) + offm) + " ") + abbr
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
        if ((String.length str) > 0) && ((_substring str 0 1) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- int ((n * 10) + (int (digits.[(_substring str i (i + 1))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
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
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and parseTime (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let c: int = s.IndexOf(":")
        let h: int = parseIntStr (_substring s 0 c)
        let mi: int = parseIntStr (_substring s (c + 1) (c + 3))
        let ampm: string = _substring s ((String.length s) - 2) (String.length s)
        let mutable hh: int = h
        if (ampm = "pm") && (h <> 12) then
            hh <- h + 12
        if (ampm = "am") && (h = 12) then
            hh <- 0
        __ret <- unbox<int array> [|hh; mi|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let input: string = "March 7 2009 7:30pm EST"
        printfn "%s" ("Input:              " + input)
        let mutable parts = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length input) do
            let ch: string = _substring input i (i + 1)
            if ch = " " then
                if (String.length cur) > 0 then
                    parts <- Array.append parts [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            parts <- Array.append parts [|cur|]
        let month: int = months.[(parts.[0])] |> unbox<int>
        let day: int = parseIntStr (parts.[1])
        let year: int = parseIntStr (parts.[2])
        let tm: int array = parseTime (parts.[3])
        let hour: int = tm.[0]
        let minute: int = tm.[1]
        let tz: string = parts.[4]
        let zoneOffsets: Map<string, int> = Map.ofList [("EST", -18000); ("EDT", -14400); ("MST", -25200)]
        let local: int = epochSeconds year month day hour minute
        let utc = local - (int (zoneOffsets.[tz] |> unbox<int>))
        let utc12 = (int utc) + 43200
        let startDST: int = epochSeconds 2009 3 8 7 0
        let mutable offEast: int = -18000
        if (int utc12) >= startDST then
            offEast <- -14400
        let eastParts: int array = fromEpoch (int ((int utc12) + offEast))
        let mutable eastAbbr: string = "EST"
        if offEast = (-14400) then
            eastAbbr <- "EDT"
        printfn "%s" ("+12 hrs:            " + (unbox<string> (formatDate eastParts offEast eastAbbr)))
        let offAZ: int = -25200
        let azParts: int array = fromEpoch (int ((int utc12) + offAZ))
        printfn "%s" ("+12 hrs in Arizona: " + (unbox<string> (formatDate azParts offAZ "MST")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
