open System
exception BreakException of int
exception ContinueException of int

exception Return_strStr of int
let strStr (haystack: string) (needle: string) : int =
    try
        let n = haystack.Length
        let m = needle.Length
        if (m = 0) then
            raise (Return_strStr (0))
        if (m > n) then
            raise (Return_strStr ((-1)))
        try
            for i = 0 to ((n - m) + 1) - 1 do
                try
                    let mutable j = 0
                    try
                        while (j < m) do
                            try
                                if ((string haystack.[(if (i + j) < 0 then haystack.Length + (i + j) else (i + j))]) <> (string needle.[(if j < 0 then needle.Length + j else j)])) then
                                    raise (BreakException 1)
                                j <- (j + 1)
                            with ContinueException n when n = 1 -> ()
                    with BreakException n when n = 1 -> ()
                    if (j = m) then
                        raise (Return_strStr (i))
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_strStr ((-1)))
        failwith "unreachable"
    with Return_strStr v -> v

