open System
open System.Text.Json

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    movie_id: int
    person_role_id: int
    role_id: int
    note: string
}
type Anon3 = {
    id: int
    country_code: string
}
type Anon4 = {
    id: int
}
type Anon5 = {
    movie_id: int
    company_id: int
    company_type_id: int
}
type Anon6 = {
    id: int
    role: string
}
type Anon7 = {
    id: int
    title: string
    production_year: int
}
type Anon8 = {
    character: obj
    movie: obj
}
type Anon9 = {
    uncredited_voiced_character: obj
    russian_movie: obj
}
type Anon10 = {
    uncredited_voiced_character: string
    russian_movie: string
}
let char_name: obj list = [{ id = 1; name = "Ivan" }; { id = 2; name = "Alex" }]
let cast_info: obj list = [{ movie_id = 10; person_role_id = 1; role_id = 1; note = "Soldier (voice) (uncredited)" }; { movie_id = 11; person_role_id = 2; role_id = 1; note = "(voice)" }]
let company_name: obj list = [{ id = 1; country_code = "[ru]" }; { id = 2; country_code = "[us]" }]
let company_type: obj list = [{ id = 1 }; { id = 2 }]
let movie_companies: obj list = [{ movie_id = 10; company_id = 1; company_type_id = 1 }; { movie_id = 11; company_id = 2; company_type_id = 1 }]
let role_type: obj list = [{ id = 1; role = "actor" }; { id = 2; role = "director" }]
let title: obj list = [{ id = 10; title = "Vodka Dreams"; production_year = 2006 }; { id = 11; title = "Other Film"; production_year = 2004 }]
let matches: obj list = [ for chn in char_name do 
  for ci in cast_info do 
  for rt in role_type do 
  for t in title do 
  for mc in movie_companies do 
  for cn in company_name do 
  for ct in company_type do if chn.id = ci.person_role_id && rt.id = ci.role_id && t.id = ci.movie_id && mc.movie_id = t.id && cn.id = mc.company_id && ct.id = mc.company_type_id && ci.note.contains("(voice)") && ci.note.contains("(uncredited)") && cn.country_code = "[ru]" && rt.role = "actor" && t.production_year > 2005 then yield { character = chn.name; movie = t.title } ]
let result: obj list = [{ uncredited_voiced_character = List.min [ for x in matches do yield x.character ]; russian_movie = List.min [ for x in matches do yield x.movie ] }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ uncredited_voiced_character = "Ivan"; russian_movie = "Vodka Dreams" }])
