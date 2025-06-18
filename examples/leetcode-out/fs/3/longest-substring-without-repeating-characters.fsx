open System
exception BreakException of int
exception ContinueException of int

exception Return_lengthOfLongestSubstring of int
let lengthOfLongestSubstring (s: string) : int =
    try
        let n = s.Length
        let mutable start = 0
        let mutable best = 0
        let mutable i = 0
        try
            while (i < n) do
                try
                    let mutable j = start
                    try
                        while (j < i) do
                            try
                                if ((string s.[(if j < 0 then s.Length + j else j)]) = (string s.[(if i < 0 then s.Length + i else i)])) then
                                    start <- (j + 1)
                                    raise (BreakException 1)
                                j <- (j + 1)
                            with ContinueException n when n = 1 -> ()
                    with BreakException n when n = 1 -> ()
                    let length = ((i - start) + 1)
                    if (length > best) then
                        best <- length
                    i <- (i + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_lengthOfLongestSubstring (best))
        failwith "unreachable"
    with Return_lengthOfLongestSubstring v -> v

