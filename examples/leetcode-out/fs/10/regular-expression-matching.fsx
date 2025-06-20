open System

exception Return_isMatch of bool
let isMatch (s: string) (p: string) : bool =
    try
        let m = s.Length
        let n = p.Length
        let mutable dp = [||]
        let mutable i = 0
        while (i <= m) do
            let mutable row = [||]
            let mutable j = 0
            while (j <= n) do
                row <- Array.append row [|false|]
                j <- (j + 1)
            dp <- Array.append dp [|row|]
            i <- (i + 1)
        dp.[m].[n] <- true
        let mutable i2 = m
        while (i2 >= 0) do
            let mutable j2 = (n - 1)
            while (j2 >= 0) do
                let mutable first = false
                if (i2 < m) then
                    if ((((string p.[(if j2 < 0 then p.Length + j2 else j2)]) = (string s.[(if i2 < 0 then s.Length + i2 else i2)]))) || (((string p.[(if j2 < 0 then p.Length + j2 else j2)]) = "."))) then
                        first <- true
                let mutable star = false
                if ((j2 + 1) < n) then
                    if ((string p.[(if (j2 + 1) < 0 then p.Length + (j2 + 1) else (j2 + 1))]) = "*") then
                        star <- true
                if star then
                    let mutable ok = false
                    if dp.[i2].[(j2 + 2)] then
                        ok <- true
                    else
                        if first then
                            if dp.[(i2 + 1)].[j2] then
                                ok <- true
                    dp.[i2].[j2] <- ok
                else
                    let mutable ok = false
                    if first then
                        if dp.[(i2 + 1)].[(j2 + 1)] then
                            ok <- true
                    dp.[i2].[j2] <- ok
                j2 <- (j2 - 1)
            i2 <- (i2 - 1)
        raise (Return_isMatch (dp.[0].[0]))
        failwith "unreachable"
    with Return_isMatch v -> v

