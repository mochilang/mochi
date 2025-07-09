open System

exception Break
exception Continue

let twoSum (nums) (target) =
    let n = List.length nums
    try
        for i in 0 .. n do
            try
                try
                    for j in i + 1 .. n do
                        try
                            if nums.[i] + nums.[j] = target then
                                [i; j]
                        with Continue -> ()
                with Break -> ()
            with Continue -> ()
    with Break -> ()
    [-1; -1]
let result = twoSum [2; 7; 11; 15] 9
printfn "%A" (result.[0])
printfn "%A" (result.[1])
