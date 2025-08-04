// Generated 2025-08-04 20:03 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type Instruction = {
    Label: string
    Opcode: string
    Arg: string
}
open System.Collections.Generic

let rec trim (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        while (start < (String.length (s))) && (((s.Substring(start, (start + 1) - start)) = " ") || ((s.Substring(start, (start + 1) - start)) = "\t")) do
            start <- start + 1
        let mutable ``end``: int = String.length (s)
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
        while i < (String.length (s)) do
            if (((String.length (sep)) > 0) && ((i + (String.length (sep))) <= (String.length (s)))) && ((_substring s i (i + (String.length (sep)))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length (sep))
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and splitWS (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable out: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if (ch = " ") || (ch = "\t") then
                if (String.length (cur)) > 0 then
                    out <- Array.append out [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length (cur)) > 0 then
            out <- Array.append out [|cur|]
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
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
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
        if ((String.length (str)) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length (str)) do
            n <- (n * 10) + (digits.[(string (str.Substring(i, (i + 1) - i)))])
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and parseAsm (asm: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, obj> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, obj>>
    let mutable asm = asm
    try
        let lines: string array = split (asm) ("\n")
        let mutable instrs: Instruction array = [||]
        let mutable labels: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable lineNum: int = 0
        let mutable i: int = 0
        while i < (Seq.length (lines)) do
            let mutable line: string = _idx lines i
            if (indexOf (line) (";")) <> (-1) then
                line <- _substring line 0 (indexOf (line) (";"))
            line <- trim (line)
            let mutable label: string = ""
            if (indexOf (line) (":")) <> (-1) then
                let idx: int = indexOf (line) (":")
                label <- trim (_substring line 0 idx)
                line <- trim (_substring line (idx + 1) (String.length (line)))
            let mutable opcode: string = ""
            let mutable arg: string = ""
            if (String.length (line)) > 0 then
                let mutable parts: string array = splitWS (line)
                if (Seq.length (parts)) > 0 then
                    opcode <- _idx parts 0
                if (Seq.length (parts)) > 1 then
                    arg <- _idx parts 1
                else
                    let ops: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("NOP", 0); ("LDA", 1); ("STA", 2); ("ADD", 3); ("SUB", 4); ("BRZ", 5); ("JMP", 6); ("STP", 7)]
                    if not (ops.ContainsKey(opcode)) then
                        arg <- opcode
                        opcode <- ""
            if label <> "" then
                labels.[label] <- lineNum
            instrs <- Array.append instrs [|{ Label = label; Opcode = opcode; Arg = arg }|]
            lineNum <- lineNum + 1
            i <- i + 1
        __ret <- unbox<System.Collections.Generic.IDictionary<string, obj>> (_dictCreate [("instructions", box (instrs)); ("labels", box (labels))])
        raise Return
        __ret
    with
        | Return -> __ret
and compile (p: System.Collections.Generic.IDictionary<string, obj>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p = p
    try
        let mutable instrs: Instruction array = unbox<Instruction array> (p.[(string ("instructions"))])
        let labels: System.Collections.Generic.IDictionary<string, int> = unbox<System.Collections.Generic.IDictionary<string, int>> (p.[(string ("labels"))])
        let mutable bytecode: int array = [||]
        let mutable i: int = 0
        let opcodes: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("NOP", 0); ("LDA", 1); ("STA", 2); ("ADD", 3); ("SUB", 4); ("BRZ", 5); ("JMP", 6); ("STP", 7)]
        while i < (Seq.length (instrs)) do
            let ins: Instruction = _idx instrs i
            let mutable arg: int = 0
            if (ins.Arg) <> "" then
                if labels.ContainsKey((ins.Arg)) then
                    arg <- labels.[(string (ins.Arg))]
                else
                    arg <- parseIntStr (ins.Arg)
            let mutable code: int = 0
            if (ins.Opcode) <> "" then
                code <- opcodes.[(string (ins.Opcode))]
            bytecode <- Array.append bytecode [|(code * 32) + arg|]
            i <- i + 1
        while (Seq.length (bytecode)) < 32 do
            bytecode <- Array.append bytecode [|0|]
        __ret <- bytecode
        raise Return
        __ret
    with
        | Return -> __ret
and floorMod (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable r: int = ((a % b + b) % b)
        if r < 0 then
            r <- r + b
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and run (bytecode: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bytecode = bytecode
    try
        let mutable acc: int = 0
        let mutable pc: int = 0
        let mutable mem: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (bytecode)) do
            mem <- Array.append mem [|_idx bytecode i|]
            i <- i + 1
        try
            while pc < 32 do
                try
                    let op: int = (_idx mem pc) / 32
                    let mutable arg: int = (((_idx mem pc) % 32 + 32) % 32)
                    pc <- pc + 1
                    if op = 0 then
                        raise Continue
                    else
                        if op = 1 then
                            acc <- _idx mem arg
                        else
                            if op = 2 then
                                mem.[arg] <- acc
                            else
                                if op = 3 then
                                    acc <- floorMod (acc + (_idx mem arg)) (256)
                                else
                                    if op = 4 then
                                        acc <- floorMod (acc - (_idx mem arg)) (256)
                                    else
                                        if op = 5 then
                                            if acc = 0 then
                                                pc <- arg
                                        else
                                            if op = 6 then
                                                pc <- arg
                                            else
                                                if op = 7 then
                                                    raise Break
                                                else
                                                    raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- acc
        raise Return
        __ret
    with
        | Return -> __ret
and execute (asm: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable asm = asm
    try
        let parsed: System.Collections.Generic.IDictionary<string, obj> = parseAsm (asm)
        let bc: int array = compile (parsed)
        __ret <- run (bc)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let examples: string array = [|((("LDA   x\n" + "ADD   y       ; accumulator = x + y\n") + "STP\n") + "x:            2\n") + "y:            2"; (((((((((((("loop:   LDA   prodt\n" + "        ADD   x\n") + "        STA   prodt\n") + "        LDA   y\n") + "        SUB   one\n") + "        STA   y\n") + "        BRZ   done\n") + "        JMP   loop\n") + "done:   LDA   prodt   ; to display it\n") + "        STP\n") + "x:            8\n") + "y:            7\n") + "prodt:        0\n") + "one:          1"; (((((((((((((((("loop:   LDA   n\n" + "        STA   temp\n") + "        ADD   m\n") + "        STA   n\n") + "        LDA   temp\n") + "        STA   m\n") + "        LDA   count\n") + "        SUB   one\n") + "        BRZ   done\n") + "        STA   count\n") + "        JMP   loop\n") + "done:   LDA   n       ; to display it\n") + "        STP\n") + "m:            1\n") + "n:            1\n") + "temp:         0\n") + "count:        8       ; valid range: 1-11\n") + "one:          1"; (((((((((((((((((((((((((((((("start:  LDA   load\n" + "ADD   car     ; head of list\n") + "STA   ldcar\n") + "ADD   one\n") + "STA   ldcdr   ; next CONS cell\n") + "ldcar:  NOP\n") + "STA   value\n") + "ldcdr:  NOP\n") + "BRZ   done    ; 0 stands for NIL\n") + "STA   car\n") + "JMP   start\n") + "done:   LDA   value   ; CAR of last CONS\n") + "STP\n") + "load:   LDA   0\n") + "value:        0\n") + "car:          28\n") + "one:          1\n") + "                        ; order of CONS cells\n") + "                        ; in memory\n") + "                        ; does not matter\n") + "        6\n") + "        0       ; 0 stands for NIL\n") + "        2       ; (CADR ls)\n") + "        26      ; (CDDR ls) -- etc.\n") + "        5\n") + "        20\n") + "        3\n") + "        30\n") + "        1       ; value of (CAR ls)\n") + "        22      ; points to (CDR ls)\n") + "        4\n") + "        24"; ((("LDA  3\n" + "SUB  4\n") + "STP  0\n") + "         0\n") + "         255"; ((("LDA  3\n" + "SUB  4\n") + "STP  0\n") + "                0\n") + "                1"; ((("LDA  3\n" + "ADD  4\n") + "STP  0\n") + "                1\n") + "                255"|]
        let mutable i: int = 0
        while i < (Seq.length (examples)) do
            let res: int = execute (_idx examples i)
            printfn "%s" (string (res))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
