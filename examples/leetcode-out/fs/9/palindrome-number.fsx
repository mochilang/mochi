open System

exception Return_isPalindrome of bool
let isPalindrome (x: int) : bool =
    try
        if (x < 0) then
            raise (Return_isPalindrome (false))
        let s = (string x)
        let n = s.Length
        for i = 0 to (n / 2) - 1 do
            if ((string s.[(if i < 0 then s.Length + i else i)]) <> (string s.[(if ((n - 1) - i) < 0 then s.Length + ((n - 1) - i) else ((n - 1) - i))])) then
                raise (Return_isPalindrome (false))
        raise (Return_isPalindrome (true))
        failwith "unreachable"
    with Return_isPalindrome v -> v

