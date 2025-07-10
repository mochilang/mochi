
printfn "%s" (String.concat " " (List.map string [1; 2; 3].[1..(3-1)]))
printfn "%s" (String.concat " " (List.map string [1; 2; 3].[0..(2-1)]))
printfn "%s" "hello".Substring(1, 4 - 1)
