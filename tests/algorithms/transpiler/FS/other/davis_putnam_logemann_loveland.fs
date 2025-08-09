// Generated 2025-08-09 10:14 +0700

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
type Clause = {
    mutable _literals: System.Collections.Generic.IDictionary<string, int>
    mutable _names: string array
}
type EvalResult = {
    mutable _value: int
    mutable _clause: Clause
}
type Formula = {
    mutable _clauses: Clause array
}
type DPLLResult = {
    mutable _sat: bool
    mutable _model: System.Collections.Generic.IDictionary<string, int>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec new_clause (lits: string array) =
    let mutable __ret : Clause = Unchecked.defaultof<Clause>
    let mutable lits = lits
    try
        let mutable m: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable _names: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (lits)) do
            let lit: string = _idx lits (int i)
            m.[lit] <- 0 - 1
            _names <- Array.append _names [|lit|]
            i <- i + 1
        __ret <- { _literals = m; _names = _names }
        raise Return
        __ret
    with
        | Return -> __ret
let rec assign_clause (c: Clause) (_model: System.Collections.Generic.IDictionary<string, int>) =
    let mutable __ret : Clause = Unchecked.defaultof<Clause>
    let mutable c = c
    let mutable _model = _model
    try
        let mutable lits: System.Collections.Generic.IDictionary<string, int> = c._literals
        let mutable i: int = 0
        while i < (Seq.length (c._names)) do
            let lit: string = _idx (c._names) (int i)
            let symbol: string = _substring lit 0 2
            if _model.ContainsKey(symbol) then
                let mutable _value: int = _dictGet _model ((string (symbol)))
                if ((_substring lit ((String.length (lit)) - 1) (String.length (lit))) = "'") && (_value <> (0 - 1)) then
                    _value <- 1 - _value
                lits.[lit] <- _value
            i <- i + 1
        c._literals <- lits
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec evaluate_clause (c: Clause) (_model: System.Collections.Generic.IDictionary<string, int>) =
    let mutable __ret : EvalResult = Unchecked.defaultof<EvalResult>
    let mutable c = c
    let mutable _model = _model
    try
        let mutable i: int = 0
        while i < (Seq.length (c._names)) do
            let lit: string = _idx (c._names) (int i)
            let sym: string = if (_substring lit ((String.length (lit)) - 1) (String.length (lit))) = "'" then (_substring lit 0 2) else (lit + "'")
            if (c._literals).ContainsKey(sym) then
                __ret <- { _value = 1; _clause = c }
                raise Return
            i <- i + 1
        c <- assign_clause (c) (_model)
        i <- 0
        while i < (Seq.length (c._names)) do
            let lit: string = _idx (c._names) (int i)
            let mutable _value: int = _dictGet (c._literals) ((string (lit)))
            if _value = 1 then
                __ret <- { _value = 1; _clause = c }
                raise Return
            if _value = (0 - 1) then
                __ret <- { _value = 0 - 1; _clause = c }
                raise Return
            i <- i + 1
        let mutable any_true: int = 0
        i <- 0
        while i < (Seq.length (c._names)) do
            let lit: string = _idx (c._names) (int i)
            if (_dictGet (c._literals) ((string (lit)))) = 1 then
                any_true <- 1
            i <- i + 1
        __ret <- { _value = any_true; _clause = c }
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_formula (cs: Clause array) =
    let mutable __ret : Formula = Unchecked.defaultof<Formula>
    let mutable cs = cs
    try
        __ret <- { _clauses = cs }
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_symbol (symbols: string array) (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable symbols = symbols
    let mutable s = s
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (symbols)) do
            if (_idx symbols (int i)) <> s then
                res <- Array.append res [|(_idx symbols (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec dpll_algorithm (_clauses: Clause array) (symbols: string array) (_model: System.Collections.Generic.IDictionary<string, int>) =
    let mutable __ret : DPLLResult = Unchecked.defaultof<DPLLResult>
    let mutable _clauses = _clauses
    let mutable symbols = symbols
    let mutable _model = _model
    try
        let mutable all_true: bool = true
        let mutable i: int = 0
        while i < (Seq.length (_clauses)) do
            let ev: EvalResult = evaluate_clause (_idx _clauses (int i)) (_model)
            _clauses <- _arrset _clauses (int i) (ev._clause)
            if (ev._value) = 0 then
                __ret <- { _sat = false; _model = unbox<System.Collections.Generic.IDictionary<string, int>> (_dictCreate []) }
                raise Return
            else
                if (ev._value) = (0 - 1) then
                    all_true <- false
            i <- i + 1
        if all_true then
            __ret <- { _sat = true; _model = _model }
            raise Return
        let p: string = _idx symbols (int 0)
        let rest: string array = remove_symbol (symbols) (p)
        let mutable tmp1: System.Collections.Generic.IDictionary<string, int> = _model
        let mutable tmp2: System.Collections.Generic.IDictionary<string, int> = _model
        tmp1.[p] <- 1
        tmp2.[p] <- 0
        let res1: DPLLResult = dpll_algorithm (_clauses) (rest) (tmp1)
        if res1._sat then
            __ret <- res1
            raise Return
        __ret <- dpll_algorithm (_clauses) (rest) (tmp2)
        raise Return
        __ret
    with
        | Return -> __ret
let rec str_clause (c: Clause) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable line: string = "{"
        let mutable first: bool = true
        let mutable i: int = 0
        while i < (Seq.length (c._names)) do
            let lit: string = _idx (c._names) (int i)
            if first then
                first <- false
            else
                line <- line + " , "
            line <- line + lit
            i <- i + 1
        line <- line + "}"
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
let rec str_formula (f: Formula) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        let mutable line: string = "{"
        let mutable i: int = 0
        while i < (Seq.length (f._clauses)) do
            line <- line + (str_clause (_idx (f._clauses) (int i)))
            if i < ((Seq.length (f._clauses)) - 1) then
                line <- line + " , "
            i <- i + 1
        line <- line + "}"
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
let clause1: Clause = new_clause (unbox<string array> [|"A4"; "A3"; "A5'"; "A1"; "A3'"|])
let clause2: Clause = new_clause (unbox<string array> [|"A4"|])
let formula: Formula = new_formula (unbox<Clause array> [|clause1; clause2|])
let formula_str: string = str_formula (formula)
let _clauses: Clause array = unbox<Clause array> [|clause1; clause2|]
let symbols: string array = unbox<string array> [|"A4"; "A3"; "A5"; "A1"|]
let mutable _model: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
let result: DPLLResult = dpll_algorithm (_clauses) (symbols) (_model)
if result._sat then
    printfn "%s" (("The formula " + formula_str) + " is satisfiable.")
else
    printfn "%s" (("The formula " + formula_str) + " is not satisfiable.")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
