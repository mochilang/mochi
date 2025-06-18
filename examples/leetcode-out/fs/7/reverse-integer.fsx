open System

exception Return_reverse of int
let reverse (x: int) : int =
    try
        let mutable sign = 1
        let mutable n = x
        if (n < 0) then
            sign <- (-1)
            n <- (-n)
        let mutable rev = 0
        while (n <> 0) do
            let digit = (n % 10)
            rev <- ((rev * 10) + digit)
            n <- (n / 10)
        rev <- (rev * sign)
        if ((rev < (((-2147483647) - 1))) || (rev > 2147483647)) then
            raise (Return_reverse (0))
        raise (Return_reverse (rev))
        failwith "unreachable"
    with Return_reverse v -> v

