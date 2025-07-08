open System

exception Break
exception Continue

let people = [dict [(name, "Alice"); (city, "Paris")]; dict [(name, "Bob"); (city, "Hanoi")]; dict [(name, "Charlie"); (city, "Paris")]; dict [(name, "Diana"); (city, "Hanoi")]; dict [(name, "Eve"); (city, "Paris")]; dict [(name, "Frank"); (city, "Hanoi")]; dict [(name, "George"); (city, "Paris")]]
let big = [ for p in people doyield dict [(city, g.key); (num, List.length g)] ]
printfn "%A" (json big)
