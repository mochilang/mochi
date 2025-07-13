open System
open System.Text.Json

type Anon1 = {
    movie_id: int
    person_id: int
}
type Anon2 = {
    id: int
    keyword: string
}
type Anon3 = {
    movie_id: int
    keyword_id: int
}
type Anon4 = {
    id: int
    name: string
}
type Anon5 = {
    id: int
    title: string
    production_year: int
}
type Anon6 = {
    movie_keyword: obj
    actor_name: obj
    marvel_movie: obj
}
type Anon7 = {
    movie_keyword: string
    actor_name: string
    marvel_movie: string
}
let cast_info: obj list = [{ movie_id = 1; person_id = 101 }; { movie_id = 2; person_id = 102 }]
let keyword: obj list = [{ id = 100; keyword = "marvel-cinematic-universe" }; { id = 200; keyword = "other" }]
let movie_keyword: obj list = [{ movie_id = 1; keyword_id = 100 }; { movie_id = 2; keyword_id = 200 }]
let name: obj list = [{ id = 101; name = "Downey Robert Jr." }; { id = 102; name = "Chris Evans" }]
let title: obj list = [{ id = 1; title = "Iron Man 3"; production_year = 2013 }; { id = 2; title = "Old Movie"; production_year = 2000 }]
let result: obj list = [ for ci in cast_info do 
  for mk in movie_keyword do 
  for k in keyword do 
  for n in name do 
  for t in title do if ci.movie_id = mk.movie_id && mk.keyword_id = k.id && ci.person_id = n.id && ci.movie_id = t.id && k.keyword = "marvel-cinematic-universe" && n.name.contains("Downey") && n.name.contains("Robert") && t.production_year > 2010 then yield { movie_keyword = k.keyword; actor_name = n.name; marvel_movie = t.title } ]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ movie_keyword = "marvel-cinematic-universe"; actor_name = "Downey Robert Jr."; marvel_movie = "Iron Man 3" }])
