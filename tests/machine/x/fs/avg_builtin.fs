open System

exception Break
exception Continue

printfn "%A" ((List.sum [1; 2; 3] / List.length [1; 2; 3]))
