open System
open System.Text.Json

type Anon1 = {
    ct_id: int
    kind: string
}
type Anon2 = {
    it_id: int
    info: string
}
type Anon3 = {
    t_id: int
    title: string
    production_year: int
}
type Anon4 = {
    movie_id: int
    company_type_id: int
    note: string
}
type Anon5 = {
    movie_id: int
    info: string
    info_type_id: int
}
type Anon6 = {
    typical_european_movie: obj
}
type Anon7 = {
    typical_european_movie: string
}
let company_type: obj list = [{ ct_id = 1; kind = "production companies" }; { ct_id = 2; kind = "other" }]
let info_type: obj list = [{ it_id = 10; info = "languages" }]
let title: obj list = [{ t_id = 100; title = "B Movie"; production_year = 2010 }; { t_id = 200; title = "A Film"; production_year = 2012 }; { t_id = 300; title = "Old Movie"; production_year = 2000 }]
let movie_companies: obj list = [{ movie_id = 100; company_type_id = 1; note = "ACME (France) (theatrical)" }; { movie_id = 200; company_type_id = 1; note = "ACME (France) (theatrical)" }; { movie_id = 300; company_type_id = 1; note = "ACME (France) (theatrical)" }]
let movie_info: obj list = [{ movie_id = 100; info = "German"; info_type_id = 10 }; { movie_id = 200; info = "Swedish"; info_type_id = 10 }; { movie_id = 300; info = "German"; info_type_id = 10 }]
let candidate_titles: obj list = [ for ct in company_type do 
  for mc in movie_companies do 
  for mi in movie_info do 
  for it in info_type do 
  for t in title do if mc.company_type_id = ct.ct_id && mi.movie_id = mc.movie_id && it.it_id = mi.info_type_id && t.t_id = mc.movie_id && List.contains List.contains ct.kind = "production companies" && "(theatrical)" mc.note && "(France)" mc.note && t.production_year > 2005 && (List.contains mi.info ["Sweden"; "Norway"; "Germany"; "Denmark"; "Swedish"; "Denish"; "Norwegian"; "German"]) then yield t.title ]
let result: obj list = [{ typical_european_movie = List.min candidate_titles }]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ typical_european_movie = "A Film" }])
