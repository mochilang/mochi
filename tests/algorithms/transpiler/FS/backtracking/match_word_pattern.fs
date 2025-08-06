// Generated 2025-08-06 16:21 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec get_value (keys: string array) (values: string array) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable keys = keys
    let mutable values = values
    let mutable key = key
    try
        let mutable i: int = 0
        while i < (Seq.length(keys)) do
            if (_idx keys (i)) = key then
                __ret <- _idx values (i)
                raise Return
            i <- i + 1
        __ret <- unbox<string> null
        raise Return
        __ret
    with
        | Return -> __ret
and contains_value (values: string array) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable values = values
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length(values)) do
            if (_idx values (i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and backtrack (pattern: string) (input_string: string) (pi: int) (si: int) (keys: string array) (values: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable pattern = pattern
    let mutable input_string = input_string
    let mutable pi = pi
    let mutable si = si
    let mutable keys = keys
    let mutable values = values
    try
        if (pi = (String.length(pattern))) && (si = (String.length(input_string))) then
            __ret <- true
            raise Return
        if (pi = (String.length(pattern))) || (si = (String.length(input_string))) then
            __ret <- false
            raise Return
        let ch: string = _substring pattern pi (pi + 1)
        let mapped: string = get_value (keys) (values) (ch)
        if mapped <> (unbox<string> null) then
            if (_substring input_string si (si + (String.length(mapped)))) = mapped then
                __ret <- backtrack (pattern) (input_string) (pi + 1) (si + (String.length(mapped))) (keys) (values)
                raise Return
            __ret <- false
            raise Return
        let mutable ``end``: int = si + 1
        try
            while ``end`` <= (String.length(input_string)) do
                try
                    let substr: string = _substring input_string si ``end``
                    if contains_value (values) (substr) then
                        ``end`` <- ``end`` + 1
                        raise Continue
                    let new_keys: string array = Array.append keys [|ch|]
                    let new_values: string array = Array.append values [|substr|]
                    if backtrack (pattern) (input_string) (pi + 1) (``end``) (new_keys) (new_values) then
                        __ret <- true
                        raise Return
                    ``end`` <- ``end`` + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and match_word_pattern (pattern: string) (input_string: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable pattern = pattern
    let mutable input_string = input_string
    try
        let keys: string array = [||]
        let values: string array = [||]
        __ret <- backtrack (pattern) (input_string) (0) (0) (keys) (values)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%b" (match_word_pattern ("aba") ("GraphTreesGraph"))
        printfn "%b" (match_word_pattern ("xyx") ("PythonRubyPython"))
        printfn "%b" (match_word_pattern ("GG") ("PythonJavaPython"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
