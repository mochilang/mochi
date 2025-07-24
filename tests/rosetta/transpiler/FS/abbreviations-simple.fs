// Generated 2025-07-24 20:52 +0700

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
        let digits = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- (n * 10) + (digits.[str.Substring(i, (i + 1) - i)])
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and isDigits (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if (String.length s) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if (ch < "0") || (ch > "9") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and readTable (table: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable table = table
    try
        let toks = fields table
        let mutable cmds: string array = [||]
        let mutable mins: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length toks) do
            let cmd = toks.[i]
            let mutable minlen: int = Seq.length cmd
            i <- i + 1
            if (i < (Seq.length toks)) && (isDigits (toks.[i])) then
                let num = parseIntStr (toks.[i])
                if (num >= 1) && (num < (Seq.length cmd)) then
                    minlen <- num
                    i <- i + 1
            cmds <- Array.append cmds [|cmd|]
            mins <- Array.append mins [|minlen|]
        __ret <- Map.ofList [("commands", box cmds); ("mins", box mins)]
        raise Return
        __ret
    with
        | Return -> __ret
and validate (commands: string array) (mins: int array) (words: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable commands = commands
    let mutable mins = mins
    let mutable words = words
    try
        let mutable results: string array = [||]
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
        let table: string = ((((((("" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
        let sentence: string = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin"
        let tbl = readTable table
        let commands = (tbl.["commands"]) :?> string array
        let mins = (tbl.["mins"]) :?> int array
        let words = fields sentence
        let results = validate commands mins words
        let mutable out1: string = "user words:"
        let mutable k: int = 0
        while k < (Seq.length words) do
            out1 <- out1 + " "
            if k < ((Seq.length words) - 1) then
                out1 <- out1 + (padRight (words.[k]) (Seq.length (results.[k])))
            else
                out1 <- out1 + (words.[k])
            k <- k + 1
        printfn "%s" out1
        printfn "%s" ("full words: " + (join results " "))
        __ret
    with
        | Return -> __ret
main()
