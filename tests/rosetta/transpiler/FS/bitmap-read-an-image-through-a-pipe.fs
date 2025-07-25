// Generated 2025-07-26 04:38 +0700

exception Return

let rec parseIntStr (str: string) =
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
            n <- (n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and splitWs (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if (((ch = " ") || (ch = "\n")) || (ch = "\t")) || (ch = "\r") then
                if (String.length cur) > 0 then
                    parts <- Array.append parts [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and parsePpm (data: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable data = data
    try
        let toks: string array = splitWs data
        if (int (Array.length toks)) < 4 then
            __ret <- Map.ofList [("err", box true)]
            raise Return
        let magic: string = toks.[0]
        let w: int = parseIntStr (unbox<string> (toks.[1]))
        let h: int = parseIntStr (unbox<string> (toks.[2]))
        let maxv: int = parseIntStr (unbox<string> (toks.[3]))
        let mutable px: int array = [||]
        let mutable i: int = 4
        while i < (int (Array.length toks)) do
            px <- Array.append px [|parseIntStr (unbox<string> (toks.[i]))|]
            i <- i + 1
        __ret <- Map.ofList [("magic", box magic); ("w", box w); ("h", box h); ("max", box maxv); ("px", box px)]
        raise Return
        __ret
    with
        | Return -> __ret
let ppmData: string = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n"
let img: Map<string, obj> = parsePpm ppmData
printfn "%s" ((("width=" + (string (img.w))) + " height=") + (string (img.h)))
