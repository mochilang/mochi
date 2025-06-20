open System

exception Return_divide of int
let divide (dividend: int) (divisor: int) : int =
    try
        if ((dividend = (((-2147483647) - 1))) && (divisor = ((-1)))) then
            raise (Return_divide (2147483647))
        let mutable negative = false
        if (dividend < 0) then
            negative <- (not negative)
            dividend <- (-dividend)
        if (divisor < 0) then
            negative <- (not negative)
            divisor <- (-divisor)
        let mutable quotient = 0
        while (dividend >= divisor) do
            let mutable temp = divisor
            let mutable multiple = 1
            while (dividend >= (temp + temp)) do
                temp <- (temp + temp)
                multiple <- (multiple + multiple)
            dividend <- (dividend - temp)
            quotient <- (quotient + multiple)
        if negative then
            quotient <- (-quotient)
        if (quotient > 2147483647) then
            raise (Return_divide (2147483647))
        if (quotient < (((-2147483647) - 1))) then
            raise (Return_divide ((-2147483648)))
        raise (Return_divide (quotient))
        failwith "unreachable"
    with Return_divide v -> v

