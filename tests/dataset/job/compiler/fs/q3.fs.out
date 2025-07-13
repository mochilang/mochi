open System
open System.Text.Json

type Anon1 = {
    id: int
    keyword: string
}
type Anon2 = {
    movie_id: int
    info: string
}
type Anon3 = {
    movie_id: int
    keyword_id: int
}
type Anon4 = {
    id: int
    title: string
    production_year: int
}
type Anon5 = {
    movie_title: obj
}
type Anon6 = {
    movie_title: string
}
let keyword: obj list = [{ id = 1; keyword = "amazing sequel" }; { id = 2; keyword = "prequel" }]
let movie_info: obj list = [{ movie_id = 10; info = "Germany" }; { movie_id = 30; info = "Sweden" }; { movie_id = 20; info = "France" }]
let movie_keyword: obj list = [{ movie_id = 10; keyword_id = 1 }; { movie_id = 30; keyword_id = 1 }; { movie_id = 20; keyword_id = 1 }; { movie_id = 10; keyword_id = 2 }]
let title: obj list = [{ id = 10; title = "Alpha"; production_year = 2006 }; { id = 30; title = "Beta"; production_year = 2008 }; { id = 20; title = "Gamma"; production_year = 2009 }]
let allowed_infos: string list = ["Sweden"; "Norway"; "Germany"; "Denmark"; "Swedish"; "Denish"; "Norwegian"; "German"]
let candidate_titles: obj list = [ for k in keyword do 
  for mk in movie_keyword do 
  for mi in movie_info do 
  for t in title do if mk.keyword_id = k.id && mi.movie_id = mk.movie_id && t.id = mi.movie_id && List.contains k.keyword.contains("sequel") && mi.info allowed_infos && t.production_year > 2005 && mk.movie_id = mi.movie_id then yield t.title ]
let result: obj list = [{ movie_title = List.min candidate_titles }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ movie_title = "Alpha" }])
