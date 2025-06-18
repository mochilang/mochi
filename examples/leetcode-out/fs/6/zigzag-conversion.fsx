open System

exception Return_convert of string
let convert (s: string) (numRows: int) : string =
    try
        if ((numRows <= 1) || (numRows >= s.Length)) then
            raise (Return_convert (s))
        let mutable rows = [||]
        let mutable i = 0
        while (i < numRows) do
            rows <- Array.append rows [|""|]
            i <- (i + 1)
        let mutable curr = 0
        let mutable step = 1
        for ch in s do
            let ch = string ch
            rows.[curr] <- (rows.[curr] + ch)
            if (curr = 0) then
                step <- 1
            elif (curr = (numRows - 1)) then
                step <- (-1)
            curr <- (curr + step)
        let mutable result = ""
        for row in rows do
            result <- (result + row)
        raise (Return_convert (result))
        failwith "unreachable"
    with Return_convert v -> v

