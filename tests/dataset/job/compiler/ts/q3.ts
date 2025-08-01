// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:38:42Z
// Source: /workspace/mochi/tests/dataset/job/q3.mochi

let allowed_infos: string[];
let candidate_titles: any[];
let keyword: Record<string, any>[];
let movie_info: Record<string, any>[];
let movie_keyword: { [key: string]: number }[];
let result: Record<string, any>[];
let title: Record<string, any>[];

function test_Q3_returns_lexicographically_smallest_sequel_title(): void {
  if (!(_equal(result, [{ "movie_title": "Alpha" }]))) {
    throw new Error("expect failed");
  }
}

function main(): void {
  keyword = [
    {
      "id": 1,
      "keyword": "amazing sequel",
    },
    {
      "id": 2,
      "keyword": "prequel",
    },
  ];
  movie_info = [
    {
      "movie_id": 10,
      "info": "Germany",
    },
    {
      "movie_id": 30,
      "info": "Sweden",
    },
    {
      "movie_id": 20,
      "info": "France",
    },
  ];
  movie_keyword = [
    {
      "movie_id": 10,
      "keyword_id": 1,
    },
    {
      "movie_id": 30,
      "keyword_id": 1,
    },
    {
      "movie_id": 20,
      "keyword_id": 1,
    },
    {
      "movie_id": 10,
      "keyword_id": 2,
    },
  ];
  title = [
    {
      "id": 10,
      "title": "Alpha",
      "production_year": 2006,
    },
    {
      "id": 30,
      "title": "Beta",
      "production_year": 2008,
    },
    {
      "id": 20,
      "title": "Gamma",
      "production_year": 2009,
    },
  ];
  allowed_infos = [
    "Sweden",
    "Norway",
    "Germany",
    "Denmark",
    "Swedish",
    "Denish",
    "Norwegian",
    "German",
  ];
  candidate_titles = (() => {
    const _src = keyword;
    const _res = [];
    for (const k of _src) {
      for (const mk of movie_keyword) {
        if (!(mk.keyword_id == k.id)) continue;
        for (const mi of movie_info) {
          if (!(mi.movie_id == mk.movie_id)) continue;
          for (const t of title) {
            if (!(t.id == mi.movie_id)) continue;
            if (
              !(((k.keyword.includes("sequel") &&
                allowed_infos.includes(mi.info)) &&
                (t.production_year > 2005)) && (mk.movie_id == mi.movie_id))
            ) continue;
            _res.push(t.title);
          }
        }
      }
    }
    return _res;
  })();
  result = [{ "movie_title": _min(candidate_titles) }];
  console.log(_json(result));
  test_Q3_returns_lexicographically_smallest_sequel_title();
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
