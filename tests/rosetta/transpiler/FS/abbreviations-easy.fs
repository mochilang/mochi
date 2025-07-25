// Generated 2025-07-25 12:29 +0700

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
        while i < (Array.length xs) do
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
        if (Array.length words) = 0 then
            __ret <- results
            raise Return
        let mutable wi: int = 0
        try
            while wi < (Array.length words) do
                let w = words.[wi]
                let mutable found: bool = false
                let wlen: int = Seq.length w
                let mutable ci: int = 0
                try
                    while ci < (Array.length commands) do
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
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
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
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
