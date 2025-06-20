open System
exception BreakException of int
exception ContinueException of int

exception Return_longestCommonPrefix of string
let longestCommonPrefix (strs: string[]) : string =
    try
        if (strs.Length = 0) then
            raise (Return_longestCommonPrefix (""))
        let mutable prefix = strs.[0]
        try
            for i = 1 to strs.Length - 1 do
                try
                    let mutable j = 0
                    let current = strs.[i]
                    try
                        while ((j < prefix.Length) && (j < current.Length)) do
                            try
                                if ((string prefix.[(if j < 0 then prefix.Length + j else j)]) <> (string current.[(if j < 0 then current.Length + j else j)])) then
                                    raise (BreakException 1)
                                j <- (j + 1)
                            with ContinueException n when n = 1 -> ()
                    with BreakException n when n = 1 -> ()
                    prefix <- prefix.[0 .. (j - 1)]
                    if (prefix = "") then
                        raise (BreakException 0)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_longestCommonPrefix (prefix))
        failwith "unreachable"
    with Return_longestCommonPrefix v -> v

