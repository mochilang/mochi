open System

exception Break
exception Continue

let people = [dict [(name, "Alice"); (age, 30)]; dict [(name, "Bob"); (age, 15)]; dict [(name, "Charlie"); (age, 65)]; dict [(name, "Diana"); (age, 45)]]
let adults = [ for person in people do if person.age >= 18 then yield dict [(name, person.name); (age, person.age); (is_senior, person.age >= 60)] ]
printfn "%s" "--- Adults ---"
try
    for person in adults do
        try
            printfn "%s" (String.concat " " [string person.name; string "is"; string person.age; string (if person.is_senior then " (senior)" else "")])
        with Continue -> ()
with Break -> ()
