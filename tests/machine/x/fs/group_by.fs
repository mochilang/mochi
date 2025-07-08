open System

exception Break
exception Continue

let people = [dict [(name, "Alice"); (age, 30); (city, "Paris")]; dict [(name, "Bob"); (age, 15); (city, "Hanoi")]; dict [(name, "Charlie"); (age, 65); (city, "Paris")]; dict [(name, "Diana"); (age, 45); (city, "Hanoi")]; dict [(name, "Eve"); (age, 70); (city, "Paris")]; dict [(name, "Frank"); (age, 22); (city, "Hanoi")]]
let stats = [ for person in people doyield dict [(city, g.key); (count, List.length g); (avg_age, (List.sum [ for p in g doyield p.age ] / List.length [ for p in g doyield p.age ]))] ]
printfn "%s" "--- People grouped by city ---"
try
    for s in stats do
        try
            printfn "%s" (String.concat " " [string s.city; string ": count ="; string s.count; string ", avg_age ="; string s.avg_age])
        with Continue -> ()
with Break -> ()
