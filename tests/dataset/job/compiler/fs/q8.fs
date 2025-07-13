open System
open System.Text.Json

type Anon1 = {
    person_id: int
    name: string
}
type Anon2 = {
    person_id: int
    movie_id: int
    note: string
    role_id: int
}
type Anon3 = {
    id: int
    country_code: string
}
type Anon4 = {
    movie_id: int
    company_id: int
    note: string
}
type Anon5 = {
    id: int
    name: string
}
type Anon6 = {
    id: int
    role: string
}
type Anon7 = {
    id: int
    title: string
}
type Anon8 = {
    pseudonym: obj
    movie_title: obj
}
type Anon9 = {
    actress_pseudonym: obj
    japanese_movie_dubbed: obj
}
type Anon10 = {
    actress_pseudonym: string
    japanese_movie_dubbed: string
}
let aka_name: obj list = [{ person_id = 1; name = "Y. S." }]
let cast_info: obj list = [{ person_id = 1; movie_id = 10; note = "(voice: English version)"; role_id = 1000 }]
let company_name: obj list = [{ id = 50; country_code = "[jp]" }]
let movie_companies: obj list = [{ movie_id = 10; company_id = 50; note = "Studio (Japan)" }]
let name: obj list = [{ id = 1; name = "Yoko Ono" }; { id = 2; name = "Yuichi" }]
let role_type: obj list = [{ id = 1000; role = "actress" }]
let title: obj list = [{ id = 10; title = "Dubbed Film" }]
let eligible: obj list = [ for an1 in aka_name do 
  for n1 in name do 
  for ci in cast_info do 
  for t in title do 
  for mc in movie_companies do 
  for cn in company_name do 
  for rt in role_type do if n1.id = an1.person_id && ci.person_id = an1.person_id && t.id = ci.movie_id && mc.movie_id = ci.movie_id && cn.id = mc.company_id && rt.id = ci.role_id && ci.note = "(voice: English version)" && cn.country_code = "[jp]" && mc.note.contains("(Japan)") && (not mc.note.contains("(USA)")) && n1.name.contains("Yo") && (not n1.name.contains("Yu")) && rt.role = "actress" then yield { pseudonym = an1.name; movie_title = t.title } ]
let result: obj list = [{ actress_pseudonym = List.min [ for x in eligible do yield x.pseudonym ]; japanese_movie_dubbed = List.min [ for x in eligible do yield x.movie_title ] }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ actress_pseudonym = "Y. S."; japanese_movie_dubbed = "Dubbed Film" }])
