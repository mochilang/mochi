// Generated 2025-07-28 07:48 +0700

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
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ((ch = " ") || (ch = "\t")) || (ch = "\n") then
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
and unescape (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = 0
        try
            while i < (String.length s) do
                try
                    if ((s.Substring(i, (i + 1) - i)) = "\\") && ((i + 1) < (String.length s)) then
                        let c: string = s.Substring(i + 1, (i + 2) - (i + 1))
                        if c = "n" then
                            out <- out + "\n"
                            i <- i + 2
                            raise Continue
                        else
                            if c = "\\" then
                                out <- out + "\\"
                                i <- i + 2
                                raise Continue
                    out <- out + (s.Substring(i, (i + 1) - i))
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and parseProgram (src: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable src = src
    try
        let lines: string array = split src "\n"
        let header: string array = fields (lines.[0])
        let dataSize: int = parseIntStr (header.[1])
        let nStrings: int = parseIntStr (header.[3])
        let mutable stringPool: string array = [||]
        let mutable i: int = 1
        while i <= nStrings do
            let s: string = lines.[i]
            if (String.length s) > 0 then
                stringPool <- Array.append stringPool [|unbox<string> (unescape (s.Substring(1, ((String.length s) - 1) - 1)))|]
            i <- i + 1
        let mutable code: Map<string, obj> array = [||]
        let mutable addrMap: Map<int, int> = Map.ofList []
        try
            while i < (Seq.length lines) do
                try
                    let line: string = trim (lines.[i])
                    if (String.length line) = 0 then
                        raise Break
                    let parts: string array = fields line
                    let addr: int = parseIntStr (parts.[0])
                    let op: string = parts.[1]
                    let mutable arg: int = 0
                    if op = "push" then
                        arg <- parseIntStr (parts.[2])
                    else
                        if (op = "fetch") || (op = "store") then
                            arg <- parseIntStr (parts.[2].Substring(1, ((String.length (parts.[2])) - 1) - 1))
                        else
                            if (op = "jmp") || (op = "jz") then
                                arg <- parseIntStr (parts.[3])
                    code <- Array.append code [|Map.ofList [("addr", box addr); ("op", box op); ("arg", box arg)]|]
                    addrMap <- Map.add addr ((Seq.length code) - 1) addrMap
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- unbox<Map<string, obj>> (Map.ofList [("dataSize", box dataSize); ("strings", box stringPool); ("code", box code); ("addrMap", box addrMap)])
        raise Return
        __ret
    with
        | Return -> __ret
and runVM (prog: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable prog = prog
    try
        let mutable data: int array = [||]
        let mutable i: int = 0
        while i < (int (prog.["dataSize"])) do
            data <- Array.append data [|0|]
            i <- i + 1
        let mutable stack: int array = [||]
        let mutable pc: int = 0
        let code: obj = box (prog.["code"])
        let addrMap: obj = box (prog.["addrMap"])
        let pool: obj = box (prog.["strings"])
        let mutable line: string = ""
        try
            while pc < (int ((unbox<System.Array> code).Length)) do
                try
                    let inst: obj = box (((code :?> System.Array).GetValue(pc)))
                    let op: obj = box (((inst :?> Map<string, obj>).["op"]))
                    let arg: obj = box (((inst :?> Map<string, obj>).["arg"]))
                    if (unbox<string> op) = "push" then
                        stack <- Array.append stack [|unbox<int> arg|]
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "store" then
                        data.[arg] <- stack.[(Seq.length stack) - 1]
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "fetch" then
                        stack <- Array.append stack [|data.[arg]|]
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "add" then
                        stack.[(Seq.length stack) - 2] <- (stack.[(Seq.length stack) - 2]) + (stack.[(Seq.length stack) - 1])
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "lt" then
                        let mutable v: int = 0
                        if (stack.[(Seq.length stack) - 2]) < (stack.[(Seq.length stack) - 1]) then
                            v <- 1
                        stack.[(Seq.length stack) - 2] <- v
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "jz" then
                        let v: int = stack.[(Seq.length stack) - 1]
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        if v = 0 then
                            pc <- int (((addrMap :?> Map<string, obj>).[arg]))
                        else
                            pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "jmp" then
                        pc <- int (((addrMap :?> Map<string, obj>).[arg]))
                        raise Continue
                    if (unbox<string> op) = "prts" then
                        let s: obj = box (((pool :?> System.Array).GetValue((stack.[(Seq.length stack) - 1]))))
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        if (unbox<string> s) <> "\n" then
                            line <- line + (unbox<string> s)
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "prti" then
                        line <- line + (string (stack.[(Seq.length stack) - 1]))
                        printfn "%s" line
                        line <- ""
                        stack <- unbox<int array> (slice stack 0 ((Seq.length stack) - 1))
                        pc <- pc + 1
                        raise Continue
                    if (unbox<string> op) = "halt" then
                        raise Break
                    pc <- pc + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and trim (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        while (start < (String.length s)) && (((s.Substring(start, (start + 1) - start)) = " ") || ((s.Substring(start, (start + 1) - start)) = "\t")) do
            start <- start + 1
        let mutable ``end``: int = String.length s
        while (``end`` > start) && (((s.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = " ") || ((s.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = "\t")) do
            ``end`` <- ``end`` - 1
        __ret <- _substring s start ``end``
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if (((String.length sep) > 0) && ((i + (String.length sep)) <= (String.length s))) && ((_substring s i (i + (String.length sep))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let programText: string = ((((((((((((((((((("Datasize: 1 Strings: 2\n" + "\"count is: \"\n") + "\"\\n\"\n") + "    0 push  1\n") + "    5 store [0]\n") + "   10 fetch [0]\n") + "   15 push  10\n") + "   20 lt\n") + "   21 jz     (43) 65\n") + "   26 push  0\n") + "   31 prts\n") + "   32 fetch [0]\n") + "   37 prti\n") + "   38 push  1\n") + "   43 prts\n") + "   44 fetch [0]\n") + "   49 push  1\n") + "   54 add\n") + "   55 store [0]\n") + "   60 jmp    (-51) 10\n") + "   65 halt\n"
        let prog: Map<string, obj> = parseProgram programText
        runVM prog
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
