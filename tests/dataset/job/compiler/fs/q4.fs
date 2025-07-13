open System
open System.Text.Json

type Anon1 = {
    id: int
    info: string
}
type Anon2 = {
    id: int
    keyword: string
}
type Anon3 = {
    id: int
    title: string
    production_year: int
}
type Anon4 = {
    movie_id: int
    keyword_id: int
}
type Anon5 = {
    movie_id: int
    info_type_id: int
    info: string
}
type Anon6 = {
    rating: obj
    title: obj
}
type Anon7 = {
    rating: obj
    movie_title: obj
}
type Anon8 = {
    rating: string
    movie_title: string
}
let info_type: obj list = [{ id = 1; info = "rating" }; { id = 2; info = "other" }]
let keyword: obj list = [{ id = 1; keyword = "great sequel" }; { id = 2; keyword = "prequel" }]
let title: obj list = [{ id = 10; title = "Alpha Movie"; production_year = 2006 }; { id = 20; title = "Beta Film"; production_year = 2007 }; { id = 30; title = "Old Film"; production_year = 2004 }]
let movie_keyword: obj list = [{ movie_id = 10; keyword_id = 1 }; { movie_id = 20; keyword_id = 1 }; { movie_id = 30; keyword_id = 1 }]
let movie_info_idx: obj list = [{ movie_id = 10; info_type_id = 1; info = "6.2" }; { movie_id = 20; info_type_id = 1; info = "7.8" }; { movie_id = 30; info_type_id = 1; info = "4.5" }]
let rows: obj list = [ for it in info_type do 
  for mi in movie_info_idx do 
  for t in title do 
  for mk in movie_keyword do 
  for k in keyword do if it.id = mi.info_type_id && t.id = mi.movie_id && mk.movie_id = t.id && k.id = mk.keyword_id && it.info = "rating" && k.keyword.contains("sequel") && mi.info > "5.0" && t.production_year > 2005 && mk.movie_id = mi.movie_id then yield { rating = mi.info; title = t.title } ]
let result: obj list = [{ rating = List.min [ for r in rows do yield r.rating ]; movie_title = List.min [ for r in rows do yield r.title ] }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ rating = "6.2"; movie_title = "Alpha Movie" }])
