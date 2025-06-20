open System
exception BreakException of int
exception ContinueException of int

exception Return_romanToInt of int
let romanToInt (s: string) : int =
    try
        let values = Map.ofList [("I", 1); ("V", 5); ("X", 10); ("L", 50); ("C", 100); ("D", 500); ("M", 1000)]
        let mutable total = 0
        let mutable i = 0
        let n = s.Length
        try
            while (i < n) do
                try
                    let curr = values.[(string s.[(if i < 0 then s.Length + i else i)])]
                    if ((i + 1) < n) then
                        let next = values.[(string s.[(if (i + 1) < 0 then s.Length + (i + 1) else (i + 1))])]
                        if (curr < next) then
                            total <- ((total + next) - curr)
                            i <- (i + 2)
                            raise (ContinueException 0)
                    total <- (total + curr)
                    i <- (i + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_romanToInt (total))
        failwith "unreachable"
    with Return_romanToInt v -> v

