open System
exception BreakException of int
exception ContinueException of int

exception Return_letterCombinations of string[]
let letterCombinations (digits: string) : string[] =
    try
        if (digits.Length = 0) then
            raise (Return_letterCombinations ([||]))
        let mapping = Map.ofList [("2", [|"a"; "b"; "c"|]); ("3", [|"d"; "e"; "f"|]); ("4", [|"g"; "h"; "i"|]); ("5", [|"j"; "k"; "l"|]); ("6", [|"m"; "n"; "o"|]); ("7", [|"p"; "q"; "r"; "s"|]); ("8", [|"t"; "u"; "v"|]); ("9", [|"w"; "x"; "y"; "z"|])]
        let mutable result = [|""|]
        try
            for d in digits do
                let d = string d
                try
                    if (not (Map.containsKey d mapping)) then
                        raise (ContinueException 0)
                    let letters = mapping.[d]
                    let next = [ for p in result do for ch in letters do yield (p + ch) ]
                    result <- next
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_letterCombinations (result))
        failwith "unreachable"
    with Return_letterCombinations v -> v

