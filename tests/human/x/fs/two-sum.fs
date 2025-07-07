let twoSum (nums:int list) (target:int) =
    let rec find i j =
        if i >= List.length nums then [-1; -1]
        elif j >= List.length nums then find (i+1) (i+2)
        elif nums.[i] + nums.[j] = target then [i; j]
        else find i (j+1)
    find 0 1

let result = twoSum [2;7;11;15] 9
printfn "%d" result.[0]
printfn "%d" result.[1]
