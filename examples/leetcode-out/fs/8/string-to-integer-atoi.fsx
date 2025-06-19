open System
exception BreakException of int
exception ContinueException of int

exception Return_digit of int
let digit (ch: string) : int =
    try
        if (ch = "0") then
            raise (Return_digit (0))
        if (ch = "1") then
            raise (Return_digit (1))
        if (ch = "2") then
            raise (Return_digit (2))
        if (ch = "3") then
            raise (Return_digit (3))
        if (ch = "4") then
            raise (Return_digit (4))
        if (ch = "5") then
            raise (Return_digit (5))
        if (ch = "6") then
            raise (Return_digit (6))
        if (ch = "7") then
            raise (Return_digit (7))
        if (ch = "8") then
            raise (Return_digit (8))
        if (ch = "9") then
            raise (Return_digit (9))
        raise (Return_digit ((-1)))
        failwith "unreachable"
    with Return_digit v -> v

exception Return_myAtoi of int
let myAtoi (s: string) : int =
    try
        let mutable i = 0
        let n = s.Length
        while ((i < n) && ((string s.[(if i < 0 then s.Length + i else i)]) = (string " ".[0]))) do
            i <- (i + 1)
        let mutable sign = 1
        if ((i < n) && ((((string s.[(if i < 0 then s.Length + i else i)]) = (string "+".[0])) || ((string s.[(if i < 0 then s.Length + i else i)]) = (string "-".[0]))))) then
            if ((string s.[(if i < 0 then s.Length + i else i)]) = (string "-".[0])) then
                sign <- (-1)
            i <- (i + 1)
        let mutable result = 0
        try
            while (i < n) do
                try
                    let ch = s.[i .. ((i + 1) - 1)]
                    let d = digit ch
                    if (d < 0) then
                        raise (BreakException 0)
                    result <- ((result * 10) + d)
                    i <- (i + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        result <- (result * sign)
        if (result > 2147483647) then
            raise (Return_myAtoi (2147483647))
        if (result < ((-2147483648))) then
            raise (Return_myAtoi ((-2147483648)))
        raise (Return_myAtoi (result))
        failwith "unreachable"
    with Return_myAtoi v -> v

