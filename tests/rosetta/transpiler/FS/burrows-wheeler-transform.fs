// Generated 2025-07-26 04:38 +0700

exception Return

let stx: string = "\x02"
let etx: string = "\x03"
let rec contains (s: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and sortStrings (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr = xs
        let mutable n: int = Array.length arr
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let tmp = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and bwt (s: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable s = s
    try
        if (contains s stx) || (contains s etx) then
            __ret <- Map.ofList [("err", box true); ("res", box "")]
            raise Return
        s <- (stx + s) + etx
        let le: int = String.length s
        let mutable table: string array = [||]
        let mutable i: int = 0
        while i < le do
            let rot: string = (s.Substring(i, le - i)) + (s.Substring(0, i - 0))
            table <- Array.append table [|rot|]
            i <- i + 1
        table <- sortStrings table
        let mutable last: string = ""
        i <- 0
        while i < le do
            last <- last + (table.[i].Substring(le - 1, le - (le - 1)))
            i <- i + 1
        __ret <- Map.ofList [("err", box false); ("res", box last)]
        raise Return
        __ret
    with
        | Return -> __ret
and ibwt (r: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    try
        let le: int = String.length r
        let mutable table: string array = [||]
        let mutable i: int = 0
        while i < le do
            table <- Array.append table [|""|]
            i <- i + 1
        let mutable n: int = 0
        while n < le do
            i <- 0
            while i < le do
                table.[i] <- (r.Substring(i, (i + 1) - i)) + (unbox<string> (table.[i]))
                i <- i + 1
            table <- sortStrings table
            n <- n + 1
        i <- 0
        while i < le do
            if (table.[i].Substring(le - 1, le - (le - 1))) = etx then
                __ret <- table.[i].Substring(1, (le - 1) - 1)
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and makePrintable (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if ch = stx then
                out <- out + "^"
            else
                if ch = etx then
                    out <- out + "|"
                else
                    out <- out + ch
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let examples: string array = [|"banana"; "appellee"; "dogwood"; "TO BE OR NOT TO BE OR WANT TO BE OR NOT?"; "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"; "\x02ABC\x03"|]
        for t in examples do
            printfn "%A" (makePrintable (unbox<string> t))
            let res: Map<string, obj> = bwt (unbox<string> t)
            if unbox<bool> (res.["err"]) then
                printfn "%s" " --> ERROR: String can't contain STX or ETX"
                printfn "%s" " -->"
            else
                let enc: string = unbox<string> (res.["res"])
                printfn "%s" (" --> " + (unbox<string> (makePrintable enc)))
                let r: string = ibwt enc
                printfn "%s" (" --> " + r)
            printfn "%s" ""
        __ret
    with
        | Return -> __ret
main()
