open System
open System.Text.Json

type Anon1 = {
    person_id: int
    name: string
}
type Anon2 = {
    id: int
    name: string
}
type Anon3 = {
    person_id: int
    person_role_id: int
    movie_id: int
    role_id: int
    note: string
}
type Anon4 = {
    id: int
    country_code: string
}
type Anon5 = {
    movie_id: int
    company_id: int
    note: string
}
type Anon6 = {
    id: int
    name: string
    gender: string
}
type Anon7 = {
    id: int
    role: string
}
type Anon8 = {
    id: int
    title: string
    production_year: int
}
type Anon9 = {
    alt: obj
    character: obj
    movie: obj
}
type Anon10 = {
    alternative_name: obj
    character_name: obj
    movie: obj
}
type Anon11 = {
    alternative_name: string
    character_name: string
    movie: string
}
let aka_name: obj list = [{ person_id = 1; name = "A. N. G." }; { person_id = 2; name = "J. D." }]
let char_name: obj list = [{ id = 10; name = "Angel" }; { id = 20; name = "Devil" }]
let cast_info: obj list = [{ person_id = 1; person_role_id = 10; movie_id = 100; role_id = 1000; note = "(voice)" }; { person_id = 2; person_role_id = 20; movie_id = 200; role_id = 1000; note = "(voice)" }]
let company_name: obj list = [{ id = 100; country_code = "[us]" }; { id = 200; country_code = "[gb]" }]
let movie_companies: obj list = [{ movie_id = 100; company_id = 100; note = "ACME Studios (USA)" }; { movie_id = 200; company_id = 200; note = "Maple Films" }]
let name: obj list = [{ id = 1; name = "Angela Smith"; gender = "f" }; { id = 2; name = "John Doe"; gender = "m" }]
let role_type: obj list = [{ id = 1000; role = "actress" }; { id = 2000; role = "actor" }]
let title: obj list = [{ id = 100; title = "Famous Film"; production_year = 2010 }; { id = 200; title = "Old Movie"; production_year = 1999 }]
let matches: obj list = [ for an in aka_name do 
  for n in name do 
  for ci in cast_info do 
  for chn in char_name do 
  for t in title do 
  for mc in movie_companies do 
  for cn in company_name do 
  for rt in role_type do if an.person_id = n.id && ci.person_id = n.id && chn.id = ci.person_role_id && t.id = ci.movie_id && mc.movie_id = t.id && cn.id = mc.company_id && rt.id = ci.role_id && (List.contains ci.note ["(voice)"; "(voice: Japanese version)"; "(voice) (uncredited)"; "(voice: English version)"]) && cn.country_code = "[us]" && (mc.note.contains("(USA)") || mc.note.contains("(worldwide)")) && n.gender = "f" && n.name.contains("Ang") && rt.role = "actress" && t.production_year >= 2005 && t.production_year <= 2015 then yield { alt = an.name; character = chn.name; movie = t.title } ]
let result: obj list = [{ alternative_name = List.min [ for x in matches do yield x.alt ]; character_name = List.min [ for x in matches do yield x.character ]; movie = List.min [ for x in matches do yield x.movie ] }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ alternative_name = "A. N. G."; character_name = "Angel"; movie = "Famous Film" }])
