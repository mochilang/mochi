open System
open System.Text.Json

type Anon1 = {
    person_id: int
    name: string
}
type Anon2 = {
    person_id: int
    movie_id: int
}
type Anon3 = {
    id: int
    info: string
}
type Anon4 = {
    id: int
    link: string
}
type Anon5 = {
    linked_movie_id: int
    link_type_id: int
}
type Anon6 = {
    id: int
    name: string
    name_pcode_cf: string
    gender: string
}
type Anon7 = {
    person_id: int
    info_type_id: int
    note: string
}
type Anon8 = {
    id: int
    title: string
    production_year: int
}
type Anon9 = {
    person_name: obj
    movie_title: obj
}
type Anon10 = {
    of_person: obj
    biography_movie: obj
}
type Anon11 = {
    of_person: string
    biography_movie: string
}
let aka_name: obj list = [{ person_id = 1; name = "Anna Mae" }; { person_id = 2; name = "Chris" }]
let cast_info: obj list = [{ person_id = 1; movie_id = 10 }; { person_id = 2; movie_id = 20 }]
let info_type: obj list = [{ id = 1; info = "mini biography" }; { id = 2; info = "trivia" }]
let link_type: obj list = [{ id = 1; link = "features" }; { id = 2; link = "references" }]
let movie_link: obj list = [{ linked_movie_id = 10; link_type_id = 1 }; { linked_movie_id = 20; link_type_id = 2 }]
let name: obj list = [{ id = 1; name = "Alan Brown"; name_pcode_cf = "B"; gender = "m" }; { id = 2; name = "Zoe"; name_pcode_cf = "Z"; gender = "f" }]
let person_info: obj list = [{ person_id = 1; info_type_id = 1; note = "Volker Boehm" }; { person_id = 2; info_type_id = 1; note = "Other" }]
let title: obj list = [{ id = 10; title = "Feature Film"; production_year = 1990 }; { id = 20; title = "Late Film"; production_year = 2000 }]
let rows: obj list = [ for an in aka_name do 
  for n in name do 
  for pi in person_info do 
  for it in info_type do 
  for ci in cast_info do 
  for t in title do 
  for ml in movie_link do 
  for lt in link_type do if n.id = an.person_id && pi.person_id = an.person_id && it.id = pi.info_type_id && ci.person_id = n.id && t.id = ci.movie_id && ml.linked_movie_id = t.id && lt.id = ml.link_type_id && (an.name.contains("a") && it.info = "mini biography" && lt.link = "features" && n.name_pcode_cf >= "A" && n.name_pcode_cf <= "F" && (n.gender = "m" || (n.gender = "f" && n.name.starts_with("B"))) && pi.note = "Volker Boehm" && t.production_year >= 1980 && t.production_year <= 1995 && pi.person_id = an.person_id && pi.person_id = ci.person_id && an.person_id = ci.person_id && ci.movie_id = ml.linked_movie_id) then yield { person_name = n.name; movie_title = t.title } ]
let result: obj list = [{ of_person = List.min [ for r in rows do yield r.person_name ]; biography_movie = List.min [ for r in rows do yield r.movie_title ] }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ of_person = "Alan Brown"; biography_movie = "Feature Film" }])
