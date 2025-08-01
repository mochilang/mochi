// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    name: string
    age: int
    city: string
}


type Anon2 = {
    name: string
    age: int
    city: string
}


type Anon3 = {
    city: obj
    count: int
    avg_age: float
}


type Anon4 = {
    person: Anon2
}


type Anon5 = {
    key: string
    items: Anon4 list
}


type Anon6 = {
    city: obj
    count: int
    avg_age: float
}


let people: Anon2 list = [{ name = "Alice"; age = 30; city = "Paris" }; { name = "Bob"; age = 15; city = "Hanoi" }; { name = "Charlie"; age = 65; city = "Paris" }; { name = "Diana"; age = 45; city = "Hanoi" }; { name = "Eve"; age = 70; city = "Paris" }; { name = "Frank"; age = 22; city = "Hanoi" }]

let stats: Anon6 list = [for  in List.groupBy (fun person -> person.city) people do

]

printfn "%s" "--- People grouped by city ---"

s

stats

printfn "%s"

(String.concat " " [string (s.city); string ": count ="; string (s.count); string ", avg_age ="; string (s.avg_age)])
