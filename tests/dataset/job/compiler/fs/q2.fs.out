open System
open System.Text.Json

type Anon1 = {
    id: int
    country_code: string
}
type Anon2 = {
    id: int
    keyword: string
}
type Anon3 = {
    movie_id: int
    company_id: int
}
type Anon4 = {
    movie_id: int
    keyword_id: int
}
type Anon5 = {
    id: int
    title: string
}
let company_name: obj list = [{ id = 1; country_code = "[de]" }; { id = 2; country_code = "[us]" }]
let keyword: obj list = [{ id = 1; keyword = "character-name-in-title" }; { id = 2; keyword = "other" }]
let movie_companies: obj list = [{ movie_id = 100; company_id = 1 }; { movie_id = 200; company_id = 2 }]
let movie_keyword: obj list = [{ movie_id = 100; keyword_id = 1 }; { movie_id = 200; keyword_id = 2 }]
let title: obj list = [{ id = 100; title = "Der Film" }; { id = 200; title = "Other Movie" }]
let titles: obj list = [ for cn in company_name do 
  for mc in movie_companies do 
  for t in title do 
  for mk in movie_keyword do 
  for k in keyword do if mc.company_id = cn.id && mc.movie_id = t.id && mk.movie_id = t.id && mk.keyword_id = k.id && cn.country_code = "[de]" && k.keyword = "character-name-in-title" && mc.movie_id = mk.movie_id then yield t.title ]
let result: obj = List.min titles
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = "Der Film")
