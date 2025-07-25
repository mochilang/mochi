// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:38:44Z
// Source: /workspace/mochi/tests/dataset/job/q30.mochi

let cast_info: Record<string, any>[];
let comp_cast_type: Record<string, any>[];
let complete_cast: { [key: string]: number }[];
let info_type: Record<string, any>[];
let keyword: Record<string, any>[];
let matches: Record<string, any>[];
let movie_info: Record<string, any>[];
let movie_info_idx: { [key: string]: number }[];
let movie_keyword: { [key: string]: number }[];
let name: Record<string, any>[];
let result: Record<string, any>[];
let title: Record<string, any>[];
let violent_keywords: string[];
let writer_notes: string[];

function test_Q30_finds_violent_horror_thriller_movies_with_male_writer(): void {
  if (
    !(_equal(result, [
      {
        "movie_budget": "Horror",
        "movie_votes": 2000,
        "writer": "John Writer",
        "complete_violent_movie": "Violent Horror",
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  comp_cast_type = [
    {
      "id": 1,
      "kind": "cast",
    },
    {
      "id": 2,
      "kind": "complete+verified",
    },
    {
      "id": 3,
      "kind": "crew",
    },
  ];
  complete_cast = [
    {
      "movie_id": 1,
      "subject_id": 1,
      "status_id": 2,
    },
    {
      "movie_id": 2,
      "subject_id": 3,
      "status_id": 2,
    },
  ];
  cast_info = [
    {
      "movie_id": 1,
      "person_id": 10,
      "note": "(writer)",
    },
    {
      "movie_id": 2,
      "person_id": 11,
      "note": "(actor)",
    },
  ];
  info_type = [
    {
      "id": 1,
      "info": "genres",
    },
    {
      "id": 2,
      "info": "votes",
    },
  ];
  keyword = [
    {
      "id": 1,
      "keyword": "murder",
    },
    {
      "id": 2,
      "keyword": "comedy",
    },
  ];
  movie_info = [
    {
      "movie_id": 1,
      "info_type_id": 1,
      "info": "Horror",
    },
    {
      "movie_id": 2,
      "info_type_id": 1,
      "info": "Comedy",
    },
  ];
  movie_info_idx = [
    {
      "movie_id": 1,
      "info_type_id": 2,
      "info": 2000,
    },
    {
      "movie_id": 2,
      "info_type_id": 2,
      "info": 150,
    },
  ];
  movie_keyword = [
    {
      "movie_id": 1,
      "keyword_id": 1,
    },
    {
      "movie_id": 2,
      "keyword_id": 2,
    },
  ];
  name = [
    {
      "id": 10,
      "name": "John Writer",
      "gender": "m",
    },
    {
      "id": 11,
      "name": "Jane Actor",
      "gender": "f",
    },
  ];
  title = [
    {
      "id": 1,
      "title": "Violent Horror",
      "production_year": 2005,
    },
    {
      "id": 2,
      "title": "Old Comedy",
      "production_year": 1995,
    },
  ];
  violent_keywords = [
    "murder",
    "violence",
    "blood",
    "gore",
    "death",
    "female-nudity",
    "hospital",
  ];
  writer_notes = [
    "(writer)",
    "(head writer)",
    "(written by)",
    "(story)",
    "(story editor)",
  ];
  matches = (() => {
    const _src = complete_cast;
    const _res = [];
    for (const cc of _src) {
      for (const cct1 of comp_cast_type) {
        if (!(cct1.id == cc.subject_id)) continue;
        for (const cct2 of comp_cast_type) {
          if (!(cct2.id == cc.status_id)) continue;
          for (const ci of cast_info) {
            if (!(ci.movie_id == cc.movie_id)) continue;
            for (const mi of movie_info) {
              if (!(mi.movie_id == cc.movie_id)) continue;
              for (const mi_idx of movie_info_idx) {
                if (!(mi_idx.movie_id == cc.movie_id)) continue;
                for (const mk of movie_keyword) {
                  if (!(mk.movie_id == cc.movie_id)) continue;
                  for (const it1 of info_type) {
                    if (!(it1.id == mi.info_type_id)) continue;
                    for (const it2 of info_type) {
                      if (!(it2.id == mi_idx.info_type_id)) continue;
                      for (const k of keyword) {
                        if (!(k.id == mk.keyword_id)) continue;
                        for (const n of name) {
                          if (!(n.id == ci.person_id)) continue;
                          for (const t of title) {
                            if (!(t.id == cc.movie_id)) continue;
                            if (
                              !((((((((([
                                "cast",
                                "crew",
                              ].includes(cct1.kind)) &&
                                (cct2.kind == "complete+verified")) &&
                                (writer_notes.includes(ci.note))) &&
                                (it1.info == "genres")) &&
                                (it2.info == "votes")) &&
                                (violent_keywords.includes(k.keyword))) && ([
                                  "Horror",
                                  "Thriller",
                                ].includes(mi.info))) && (n.gender == "m")) &&
                                (t.production_year > 2000))
                            ) continue;
                            _res.push({
                              "budget": mi.info,
                              "votes": mi_idx.info,
                              "writer": n.name,
                              "movie": t.title,
                            });
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    return _res;
  })();
  result = [
    {
      "movie_budget": _min(matches.map((x) => x.budget)),
      "movie_votes": _min(matches.map((x) => x.votes)),
      "writer": _min(matches.map((x) => x.writer)),
      "complete_violent_movie": _min(matches.map((x) => x.movie)),
    },
  ];
  console.log(_json(result));
  test_Q30_finds_violent_horror_thriller_movies_with_male_writer();
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

function _json(v: any): string {
  function _sort(x: any): any {
    if (Array.isArray(x)) return x.map(_sort);
    if (x && typeof x === "object") {
      const keys = Object.keys(x).sort();
      const o: any = {};
      for (const k of keys) o[k] = _sort(x[k]);
      return o;
    }
    return x;
  }
  return JSON.stringify(_sort(v), null, 2);
}

function _min(v: any): any {
  let list: any[] | null = null;
  if (Array.isArray(v)) list = v;
  else if (v && typeof v === "object") {
    if (Array.isArray((v as any).items)) list = (v as any).items;
    else if (Array.isArray((v as any).Items)) list = (v as any).Items;
  }
  if (!list || list.length === 0) return 0;
  let m: any = list[0];
  if (typeof m === "string") {
    for (const s of list) if (typeof s === "string" && s < m) m = s;
    return m;
  }
  let mv = Number(m);
  for (const n of list) {
    const num = Number(n);
    if (num < mv) mv = num;
  }
  return mv;
}

main();
