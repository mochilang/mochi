// Generated 2025-07-24 20:06 +0700

exception Break
exception Continue

exception Return

let rec fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if ((ch = " ") || (ch = "\n")) || (ch = "\t") then
                if (String.length cur) > 0 then
                    words <- Array.append words [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            words <- Array.append words [|cur|]
        __ret <- words
        raise Return
        __ret
    with
        | Return -> __ret
and padRight (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        let mutable i: int = String.length s
        while i < width do
            out <- out + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and validate (commands: string array) (words: string array) (mins: int array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable commands = commands
    let mutable words = words
    let mutable mins = mins
    try
        let mutable results: string array = [||]
        if (Seq.length words) = 0 then
            __ret <- results
            raise Return
        let mutable wi: int = 0
        try
            while wi < (Seq.length words) do
                let w = words.[wi]
                let mutable found: bool = false
                let wlen: int = Seq.length w
                let mutable ci: int = 0
                try
                    while ci < (Seq.length commands) do
                        let cmd = commands.[ci]
                        if (((mins.[ci]) <> 0) && (wlen >= (mins.[ci]))) && (wlen <= (Seq.length cmd)) then
                            let c = cmd.ToUpper()
                            let ww = w.ToUpper()
                            if (c.Substring(0, wlen - 0)) = ww then
                                results <- Array.append results [|c|]
                                found <- true
                                raise Break
                        ci <- ci + 1
                with
                | Break -> ()
                | Continue -> ()
                if not found then
                    results <- Array.append results [|"*error*"|]
                wi <- wi + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- results
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let table: string = ((((("Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ") + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ") + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ") + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ") + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ") + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp "
        let commands = fields table
        let mutable mins: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length commands) do
            let mutable count: int = 0
            let mutable j: int = 0
            let cmd = commands.[i]
            while j < (Seq.length cmd) do
                let ch: string = cmd.Substring(j, (j + 1) - j)
                if (ch >= "A") && (ch <= "Z") then
                    count <- count + 1
                j <- j + 1
            mins <- Array.append mins [|count|]
            i <- i + 1
        let sentence: string = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
        let words = fields sentence
        let results = validate commands words mins
        let mutable out1: string = "user words:  "
        let mutable k: int = 0
        while k < (Seq.length words) do
            out1 <- (out1 + (padRight (words.[k]) (Seq.length (results.[k])))) + " "
            k <- k + 1
        printfn "%s" out1
        printfn "%s" ("full words:  " + (join results " "))
        __ret
    with
        | Return -> __ret
main()
