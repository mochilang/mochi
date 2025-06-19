open System
exception BreakException of int
exception ContinueException of int

exception Return_expand of int
let expand (s: string) (left: int) (right: int) : int =
    try
        let mutable l = left
        let mutable r = right
        let n = s.Length
        try
            while ((l >= 0) && (r < n)) do
                try
                    if ((string s.[(if l < 0 then s.Length + l else l)]) <> (string s.[(if r < 0 then s.Length + r else r)])) then
                        raise (BreakException 0)
                    l <- (l - 1)
                    r <- (r + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_expand (((r - l) - 1)))
        failwith "unreachable"
    with Return_expand v -> v

exception Return_longestPalindrome of string
let longestPalindrome (s: string) : string =
    try
        if (s.Length <= 1) then
            raise (Return_longestPalindrome (s))
        let mutable start = 0
        let mutable _end = 0
        let n = s.Length
        for i = 0 to n - 1 do
            let len1 = expand s i i
            let len2 = expand s i (i + 1)
            let mutable l = len1
            if (len2 > len1) then
                l <- len2
            if (l > ((_end - start))) then
                start <- (i - ((((l - 1)) / 2)))
                _end <- (i + ((l / 2)))
        raise (Return_longestPalindrome (s.[start .. ((_end + 1) - 1)]))
        failwith "unreachable"
    with Return_longestPalindrome v -> v

