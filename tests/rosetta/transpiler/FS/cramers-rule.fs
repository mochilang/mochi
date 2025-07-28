// Generated 2025-07-28 11:14 +0700

exception Return

let rec det (m: float array array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        let n: int = Seq.length m
        if n = 1 then
            __ret <- (m.[0]).[0]
            raise Return
        let mutable total: float = 0.0
        let mutable sign: float = 1.0
        let mutable c: int = 0
        while c < n do
            let mutable sub: float array array = [||]
            let mutable r: int = 1
            while r < n do
                let mutable row: float array = [||]
                let mutable cc: int = 0
                while cc < n do
                    if cc <> c then
                        row <- Array.append row [|(m.[r]).[cc]|]
                    cc <- cc + 1
                sub <- Array.append sub [|row|]
                r <- r + 1
            total <- total + (float ((sign * ((m.[0]).[c])) * (float (det sub))))
            sign <- sign * (-1.0)
            c <- c + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and replaceCol (m: float array array) (col: int) (v: float array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    let mutable col = col
    let mutable v = v
    try
        let mutable res: float array array = [||]
        let mutable r: int = 0
        while r < (Seq.length m) do
            let mutable row: float array = [||]
            let mutable c: int = 0
            while c < (Seq.length (m.[r])) do
                if c = col then
                    row <- Array.append row [|v.[r]|]
                else
                    row <- Array.append row [|(m.[r]).[c]|]
                c <- c + 1
            res <- Array.append res [|row|]
            r <- r + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let m: float array array = [|[|2.0; -1.0; 5.0; 1.0|]; [|3.0; 2.0; 2.0; -6.0|]; [|1.0; 3.0; 3.0; -1.0|]; [|5.0; -2.0; -3.0; 3.0|]|]
let v: float array = [|-3.0; -32.0; -47.0; 49.0|]
let d: float = det m
let mutable x: float array = [||]
let mutable i: int = 0
while i < (Seq.length v) do
    let mc: float array array = replaceCol m i v
    x <- Array.append x [|unbox<float> ((float (det mc)) / d)|]
    i <- i + 1
let mutable s: string = "["
let mutable j: int = 0
while j < (Seq.length x) do
    s <- s + (string (x.[j]))
    if j < ((Seq.length x) - 1) then
        s <- s + " "
    j <- j + 1
s <- s + "]"
printfn "%s" s
