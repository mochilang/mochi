// Generated 2025-07-24 07:28 +0700

exception Return

let mutable _nowSeed = 0
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
type Rational = {
    num: int
    denom: int
}
type Expr =
    | Num of Rational
    | Bin of int * Expr * Expr
open System

let OP_ADD: int = 1
let OP_SUB: int = 2
let OP_MUL: int = 3
let OP_DIV: int = 4
let rec binEval (op: int) (l: Expr) (r: Expr) =
    let mutable __ret : Rational = Unchecked.defaultof<Rational>
    let mutable op = op
    let mutable l = l
    let mutable r = r
    try
        let lv = exprEval l
        let rv = exprEval r
        if op = OP_ADD then
            __ret <- { num = ((lv.num) * (rv.denom)) + ((lv.denom) * (rv.num)); denom = (lv.denom) * (rv.denom) }
            raise Return
        if op = OP_SUB then
            __ret <- { num = ((lv.num) * (rv.denom)) - ((lv.denom) * (rv.num)); denom = (lv.denom) * (rv.denom) }
            raise Return
        if op = OP_MUL then
            __ret <- { num = (lv.num) * (rv.num); denom = (lv.denom) * (rv.denom) }
            raise Return
        __ret <- { num = (lv.num) * (rv.denom); denom = (lv.denom) * (rv.num) }
        raise Return
        __ret
    with
        | Return -> __ret
and binString (op: int) (l: Expr) (r: Expr) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable op = op
    let mutable l = l
    let mutable r = r
    try
        let ls = exprString l
        let rs = exprString r
        let mutable opstr: string = ""
        if op = OP_ADD then
            opstr <- " + "
        else
            if op = OP_SUB then
                opstr <- " - "
            else
                if op = OP_MUL then
                    opstr <- " * "
                else
                    opstr <- " / "
        __ret <- ((("(" + ls) + opstr) + rs) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and newNum (n: int) =
    let mutable __ret : Expr = Unchecked.defaultof<Expr>
    let mutable n = n
    try
        __ret <- Num({ num = n; denom = 1 })
        raise Return
        __ret
    with
        | Return -> __ret
and exprEval (x: Expr) =
    let mutable __ret : Rational = Unchecked.defaultof<Rational>
    let mutable x = x
    try
        __ret <- (match x with
            | Num(v) -> v
            | Bin(op, l, r) -> binEval op l r)
        raise Return
        __ret
    with
        | Return -> __ret
and exprString (x: Expr) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        __ret <- (match x with
            | Num(v) -> string (v.num)
            | Bin(op, l, r) -> binString op l r)
        raise Return
        __ret
    with
        | Return -> __ret
let n_cards: int = 4
let goal: int = 24
let digit_range: int = 9
let rec solve (xs: Expr array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    try
        if (Seq.length xs) = 1 then
            let f = exprEval (xs.[0])
            if ((f.denom) <> 0) && ((f.num) = ((f.denom) * goal)) then
                printfn "%s" (string (exprString (xs.[0])))
                __ret <- true
                raise Return
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length xs) do
            let mutable j: int = i + 1
            while j < (Seq.length xs) do
                let mutable rest = [||]
                let mutable k: int = 0
                while k < (Seq.length xs) do
                    if (k <> i) && (k <> j) then
                        rest <- Array.append rest [|xs.[k]|]
                    k <- k + 1
                let a = xs.[i]
                let b = xs.[j]
                for op in [|OP_ADD; OP_SUB; OP_MUL; OP_DIV|] do
                    let mutable node: Expr = Bin(op, a, b)
                    if solve (Array.append rest [|node|]) then
                        __ret <- true
                        raise Return
                let mutable node: Expr = Bin(OP_SUB, b, a)
                if solve (Array.append rest [|node|]) then
                    __ret <- true
                    raise Return
                node <- Bin(OP_DIV, b, a)
                if solve (Array.append rest [|node|]) then
                    __ret <- true
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable iter: int = 0
        while iter < 10 do
            let mutable cards = [||]
            let mutable i: int = 0
            while i < n_cards do
                let n = ((_now()) % (digit_range - 1)) + 1
                cards <- Array.append cards [|newNum n|]
                printfn "%s" (" " + (string n))
                i <- i + 1
            printfn "%s" ":  "
            if not (solve cards) then
                printfn "%s" "No solution"
            iter <- iter + 1
        __ret
    with
        | Return -> __ret
main()
