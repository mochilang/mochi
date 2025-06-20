open System

exception Return_backtrack of obj

exception Return_generateParenthesis of string[]
let generateParenthesis (n: int) : string[] =
    try
        let mutable result = [||]
        let rec backtrack (current: string) (open: int) (close: int) : obj =
            try
                if (current.Length = (n * 2)) then
                    result <- Array.append result [|current|]
                else
                    if (open < n) then
                        backtrack (current + "(") (open + 1) close
                    if (close < open) then
                        backtrack (current + ")") open (close + 1)
                failwith "unreachable"
            with Return_backtrack v -> v
        backtrack "" 0 0
        raise (Return_generateParenthesis (result))
        failwith "unreachable"
    with Return_generateParenthesis v -> v

