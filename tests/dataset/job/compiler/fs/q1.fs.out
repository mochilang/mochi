open System
open System.Text.Json

type Anon1 = {
    id: int
    kind: string
}
type Anon2 = {
    id: int
    info: string
}
type Anon3 = {
    id: int
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
    info_type_id: int
}
type Anon6 = {
    note: obj
    title: obj
    year: obj
}
type Anon7 = {
    production_note: obj
    movie_title: obj
    movie_year: obj
}
type Anon8 = {
    production_note: string
    movie_title: string
    movie_year: int
}
let company_type: obj list = [{ id = 1; kind = "production companies" }; { id = 2; kind = "distributors" }]
let info_type: obj list = [{ id = 10; info = "top 250 rank" }; { id = 20; info = "bottom 10 rank" }]
let title: obj list = [{ id = 100; title = "Good Movie"; production_year = 1995 }; { id = 200; title = "Bad Movie"; production_year = 2000 }]
let movie_companies: obj list = [{ movie_id = 100; company_type_id = 1; note = "ACME (co-production)" }; { movie_id = 200; company_type_id = 1; note = "MGM (as Metro-Goldwyn-Mayer Pictures)" }]
let movie_info_idx: obj list = [{ movie_id = 100; info_type_id = 10 }; { movie_id = 200; info_type_id = 20 }]
let filtered: obj list = [ for ct in company_type do 
  for mc in movie_companies do 
  for t in title do 
  for mi in movie_info_idx do 
  for it in info_type do if ct.id = mc.company_type_id && t.id = mc.movie_id && mi.movie_id = t.id && it.id = mi.info_type_id && ct.kind = "production companies" && it.info = "top 250 rank" && (not mc.note.contains("(as Metro-Goldwyn-Mayer Pictures)")) && (mc.note.contains("(co-production)") || mc.note.contains("(presents)")) then yield { note = mc.note; title = t.title; year = t.production_year } ]
let result: Anon7 = { production_note = List.min [ for r in filtered do yield r.note ]; movie_title = List.min [ for r in filtered do yield r.title ]; movie_year = List.min [ for r in filtered do yield r.year ] }
printfn "%A" (JsonSerializer.Serialize([result]))
assert (result = { production_note = "ACME (co-production)"; movie_title = "Good Movie"; movie_year = 1995 })
