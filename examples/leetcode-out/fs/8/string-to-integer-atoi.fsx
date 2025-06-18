open System
exception BreakException of int
exception ContinueException of int

exception Return_myAtoi of int
let myAtoi (s: string) : int =
    try
        let mutable i = 0
        let n = s.Length
        while ((i < n) && ((string s.[(if i < 0 then s.Length + i else i)]) = " ")) do
            i <- (i + 1)
        let mutable sign = 1
        if ((i < n) && ((((string s.[(if i < 0 then s.Length + i else i)]) = "+") || ((string s.[(if i < 0 then s.Length + i else i)]) = "-")))) then
            if ((string s.[(if i < 0 then s.Length + i else i)]) = "-") then
                sign <- (-1)
            i <- (i + 1)
        let digits = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        let mutable result = 0
        try
            while (i < n) do
                try
                    let ch = (string s.[(if i < 0 then s.Length + i else i)])
                    if (not (Map.containsKey ch digits)) then
                        raise (BreakException 0)
                    let d = digits.[ch]
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

