// Generated 2025-07-21 21:44 +0700

type Person = {
    name: string
    age: int
    status: string
}
let people: struct list = [{ name = "Alice"; age = 17; status = "minor" }; { name = "Bob"; age = 25; status = "unknown" }; { name = "Charlie"; age = 18; status = "unknown" }; { name = "Diana"; age = 16; status = "minor" }]
people <- List.map (fun item -> if age >= 18 then { { item with status = "adult" } with age = age + 1 } else item) people
printfn "%s" "ok"
