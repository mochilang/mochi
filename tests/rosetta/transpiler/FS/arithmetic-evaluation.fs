// Generated 2025-07-25 00:53 +0700

exception Break
exception Continue

exception Return

type Parser = {
    expr: string
    pos: int
}
type Res = {
    v: int
    p: Parser
}
let rec skipWS (p: Parser) =
    let mutable __ret : Parser = Unchecked.defaultof<Parser>
    let mutable p = p
    try
        let mutable i: int = p.pos
        while (i < (String.length (p.expr))) && ((p.expr.Substring(i, (i + 1) - i)) = " ") do
            i <- i + 1
        { p with pos = i }
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable n: int = 0
        while i < (String.length str) do
            n <- ((n * 10) + (int (str.Substring(i, (i + 1) - i)))) - 48
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and parseNumber (p: Parser) =
    let mutable __ret : Res = Unchecked.defaultof<Res>
    let mutable p = p
    try
        p <- skipWS p
        let mutable start: int = p.pos
        try
            while (p.pos) < (String.length (p.expr)) do
                let ch: string = p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))
                if (ch >= "0") && (ch <= "9") then
                    { p with pos = (p.pos) + 1 }
                else
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        let token: string = p.expr.Substring(start, (p.pos) - start)
        __ret <- { v = parseIntStr token; p = p }
        raise Return
        __ret
    with
        | Return -> __ret
and parseFactor (p: Parser) =
    let mutable __ret : Res = Unchecked.defaultof<Res>
    let mutable p = p
    try
        p <- skipWS p
        if ((p.pos) < (String.length (p.expr))) && ((p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))) = "(") then
            { p with pos = (p.pos) + 1 }
            let mutable r = parseExpr p
            let mutable v = r.v
            p <- r.p
            p <- skipWS p
            if ((p.pos) < (String.length (p.expr))) && ((p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))) = ")") then
                { p with pos = (p.pos) + 1 }
            __ret <- { v = v; p = p }
            raise Return
        if ((p.pos) < (String.length (p.expr))) && ((p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))) = "-") then
            { p with pos = (p.pos) + 1 }
            let mutable r = parseFactor p
            let mutable v = r.v
            p <- r.p
            __ret <- { v = -v; p = p }
            raise Return
        __ret <- parseNumber p
        raise Return
        __ret
    with
        | Return -> __ret
and powInt (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable r: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                r <- r * b
            b <- b * b
            e <- e / (int 2)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and parsePower (p: Parser) =
    let mutable __ret : Res = Unchecked.defaultof<Res>
    let mutable p = p
    try
        let mutable r = parseFactor p
        let mutable v = r.v
        p <- r.p
        try
            while true do
                p <- skipWS p
                if ((p.pos) < (String.length (p.expr))) && ((p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))) = "^") then
                    { p with pos = (p.pos) + 1 }
                    let mutable r2 = parseFactor p
                    let mutable rhs = r2.v
                    p <- r2.p
                    v <- powInt v rhs
                else
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { v = v; p = p }
        raise Return
        __ret
    with
        | Return -> __ret
and parseTerm (p: Parser) =
    let mutable __ret : Res = Unchecked.defaultof<Res>
    let mutable p = p
    try
        let mutable r = parsePower p
        let mutable v = r.v
        p <- r.p
        try
            while true do
                p <- skipWS p
                if (p.pos) < (String.length (p.expr)) then
                    let op: string = p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))
                    if op = "*" then
                        { p with pos = (p.pos) + 1 }
                        let mutable r2 = parsePower p
                        let mutable rhs = r2.v
                        p <- r2.p
                        v <- v * rhs
                        raise Continue
                    if op = "/" then
                        { p with pos = (p.pos) + 1 }
                        let mutable r2 = parsePower p
                        let mutable rhs = r2.v
                        p <- r2.p
                        v <- v / (int rhs)
                        raise Continue
                raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { v = v; p = p }
        raise Return
        __ret
    with
        | Return -> __ret
and parseExpr (p: Parser) =
    let mutable __ret : Res = Unchecked.defaultof<Res>
    let mutable p = p
    try
        let mutable r = parseTerm p
        let mutable v = r.v
        p <- r.p
        try
            while true do
                p <- skipWS p
                if (p.pos) < (String.length (p.expr)) then
                    let op: string = p.expr.Substring(p.pos, ((p.pos) + 1) - (p.pos))
                    if op = "+" then
                        { p with pos = (p.pos) + 1 }
                        let mutable r2 = parseTerm p
                        let mutable rhs = r2.v
                        p <- r2.p
                        v <- v + rhs
                        raise Continue
                    if op = "-" then
                        { p with pos = (p.pos) + 1 }
                        let mutable r2 = parseTerm p
                        let mutable rhs = r2.v
                        p <- r2.p
                        v <- v - rhs
                        raise Continue
                raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { v = v; p = p }
        raise Return
        __ret
    with
        | Return -> __ret
and evalExpr (expr: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable expr = expr
    try
        let mutable p: Parser = { expr = expr; pos = 0 }
        let r = parseExpr p
        __ret <- r.v
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let expr: string = "2*(3-1)+2*5"
        printfn "%s" ((expr + " = ") + (string (evalExpr expr)))
        __ret
    with
        | Return -> __ret
main()
