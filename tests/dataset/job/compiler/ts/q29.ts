// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:38:44Z
// Source: /workspace/mochi/tests/dataset/job/q29.mochi

let aka_name: { [key: string]: number }[];
let cast_info: Record<string, any>[];
let char_name: Record<string, any>[];
let comp_cast_type: Record<string, any>[];
let company_name: Record<string, any>[];
let complete_cast: { [key: string]: number }[];
let info_type: Record<string, any>[];
let keyword: Record<string, any>[];
let matches: Record<string, any>[];
let movie_companies: { [key: string]: number }[];
let movie_info: Record<string, any>[];
let movie_keyword: { [key: string]: number }[];
let name: Record<string, any>[];
let person_info: { [key: string]: number }[];
let result: Record<string, any>[];
let role_type: Record<string, any>[];
let title: Record<string, any>[];

function test_Q29_finds_the_actress_voicing_the_Queen_in_Shrek_2(): void {
  if (
    !(_equal(result, [
      {
        "voiced_char": "Queen",
        "voicing_actress": "Angela Aniston",
        "voiced_animation": "Shrek 2",
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  aka_name = [
    { "person_id": 1 },
    { "person_id": 2 },
  ];
  complete_cast = [
    {
      "movie_id": 1,
      "subject_id": 1,
      "status_id": 2,
    },
    {
      "movie_id": 2,
      "subject_id": 1,
      "status_id": 2,
    },
  ];
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
      "kind": "other",
    },
  ];
  char_name = [
    {
      "id": 1,
      "name": "Queen",
    },
    {
      "id": 2,
      "name": "Princess",
    },
  ];
  cast_info = [
    {
      "movie_id": 1,
      "person_id": 1,
      "role_id": 1,
      "person_role_id": 1,
      "note": "(voice)",
    },
    {
      "movie_id": 2,
      "person_id": 2,
      "role_id": 1,
      "person_role_id": 2,
      "note": "(voice)",
    },
  ];
  company_name = [
    {
      "id": 1,
      "country_code": "[us]",
    },
    {
      "id": 2,
      "country_code": "[uk]",
    },
  ];
  info_type = [
    {
      "id": 1,
      "info": "release dates",
    },
    {
      "id": 2,
      "info": "trivia",
    },
    {
      "id": 3,
      "info": "other",
    },
  ];
  keyword = [
    {
      "id": 1,
      "keyword": "computer-animation",
    },
    {
      "id": 2,
      "keyword": "action",
    },
  ];
  movie_companies = [
    {
      "movie_id": 1,
      "company_id": 1,
    },
    {
      "movie_id": 2,
      "company_id": 2,
    },
  ];
  movie_info = [
    {
      "movie_id": 1,
      "info_type_id": 1,
      "info": "USA:2004",
    },
    {
      "movie_id": 2,
      "info_type_id": 1,
      "info": "USA:1995",
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
      "id": 1,
      "name": "Angela Aniston",
      "gender": "f",
    },
    {
      "id": 2,
      "name": "Bob Brown",
      "gender": "m",
    },
  ];
  person_info = [
    {
      "person_id": 1,
      "info_type_id": 2,
    },
    {
      "person_id": 2,
      "info_type_id": 2,
    },
  ];
  role_type = [
    {
      "id": 1,
      "role": "actress",
    },
    {
      "id": 2,
      "role": "actor",
    },
  ];
  title = [
    {
      "id": 1,
      "title": "Shrek 2",
      "production_year": 2004,
    },
    {
      "id": 2,
      "title": "Old Film",
      "production_year": 1999,
    },
  ];
  matches = (() => {
    const _src = aka_name;
    const _res = [];
    for (const an of _src) {
      for (const cc of complete_cast) {
        for (const cct1 of comp_cast_type) {
          for (const cct2 of comp_cast_type) {
            for (const chn of char_name) {
              for (const ci of cast_info) {
                for (const cn of company_name) {
                  for (const it of info_type) {
                    for (const it3 of info_type) {
                      for (const k of keyword) {
                        for (const mc of movie_companies) {
                          for (const mi of movie_info) {
                            for (const mk of movie_keyword) {
                              for (const n of name) {
                                for (const pi of person_info) {
                                  for (const rt of role_type) {
                                    for (const t of title) {
                                      if (
                                        !(((((((((((((((((((((((((((((((((((((((((((cct1
                                          .kind == "cast") &&
                                          (cct2.kind == "complete+verified")) &&
                                          (chn.name == "Queen")) &&
                                          (((ci.note == "(voice)") ||
                                            (ci.note ==
                                              "(voice) (uncredited)")) ||
                                            (ci.note ==
                                              "(voice: English version)"))) &&
                                          (cn.country_code == "[us]")) &&
                                          (it.info == "release dates")) &&
                                          (it3.info == "trivia")) &&
                                          (k.keyword ==
                                            "computer-animation")) &&
                                          (_starts_with(mi.info, "Japan:200") ||
                                            _starts_with(
                                              mi.info,
                                              "USA:200",
                                            ))) && (n.gender == "f")) &&
                                          n.name.includes("An")) &&
                                          (rt.role == "actress")) &&
                                          (t.title == "Shrek 2")) &&
                                          (t.production_year >= 2000)) &&
                                          (t.production_year <= 2010)) &&
                                          (t.id == mi.movie_id)) &&
                                          (t.id == mc.movie_id)) &&
                                          (t.id == ci.movie_id)) &&
                                          (t.id == mk.movie_id)) &&
                                          (t.id == cc.movie_id)) &&
                                          (mc.movie_id == ci.movie_id)) &&
                                          (mc.movie_id == mi.movie_id)) &&
                                          (mc.movie_id == mk.movie_id)) &&
                                          (mc.movie_id == cc.movie_id)) &&
                                          (mi.movie_id == ci.movie_id)) &&
                                          (mi.movie_id == mk.movie_id)) &&
                                          (mi.movie_id == cc.movie_id)) &&
                                          (ci.movie_id == mk.movie_id)) &&
                                          (ci.movie_id == cc.movie_id)) &&
                                          (mk.movie_id == cc.movie_id)) &&
                                          (cn.id == mc.company_id)) &&
                                          (it.id == mi.info_type_id)) &&
                                          (n.id == ci.person_id)) &&
                                          (rt.id == ci.role_id)) &&
                                          (n.id == an.person_id)) &&
                                          (ci.person_id == an.person_id)) &&
                                          (chn.id == ci.person_role_id)) &&
                                          (n.id == pi.person_id)) &&
                                          (ci.person_id == pi.person_id)) &&
                                          (it3.id == pi.info_type_id)) &&
                                          (k.id == mk.keyword_id)) &&
                                          (cct1.id == cc.subject_id)) &&
                                          (cct2.id == cc.status_id))
                                      ) continue;
                                      _res.push({
                                        "voiced_char": chn.name,
                                        "voicing_actress": n.name,
                                        "voiced_animation": t.title,
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
            }
          }
        }
      }
    }
    return _res;
  })();
  result = [
    {
      "voiced_char": _min(matches.map((x) => x.voiced_char)),
      "voicing_actress": _min(matches.map((x) => x.voicing_actress)),
      "voiced_animation": _min(matches.map((x) => x.voiced_animation)),
    },
  ];
  console.log(_json(result));
  test_Q29_finds_the_actress_voicing_the_Queen_in_Shrek_2();
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

function _starts_with(str: any, prefix: any): boolean {
  return String(str).startsWith(String(prefix));
}

main();
