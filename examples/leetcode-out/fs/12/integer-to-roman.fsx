open System

exception Return_intToRoman of string
let intToRoman (num: int) : string =
    try
        let values = [|1000; 900; 500; 400; 100; 90; 50; 40; 10; 9; 5; 4; 1|]
        let symbols = [|"M"; "CM"; "D"; "CD"; "C"; "XC"; "L"; "XL"; "X"; "IX"; "V"; "IV"; "I"|]
        let mutable result = ""
        let mutable i = 0
        while (num > 0) do
            while (num >= values.[i]) do
                result <- (result + symbols.[i])
                num <- (num - values.[i])
            i <- (i + 1)
        raise (Return_intToRoman (result))
        failwith "unreachable"
    with Return_intToRoman v -> v

