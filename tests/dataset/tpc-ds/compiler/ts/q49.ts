// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:15Z
// Source: tests/dataset/tpc-ds/q49.mochi

let catalog: Record<string, any>[];
let result: any[];
let store: Record<string, any>[];
let tmp: any[];
let web: Record<string, any>[];

function test_TPCDS_Q49_simplified(): void {
  if (
    !(_equal(result, [
      {
        "channel": "catalog",
        "item": "A",
        "return_ratio": 0.3,
        "return_rank": 1,
        "currency_rank": 1,
      },
      {
        "channel": "store",
        "item": "A",
        "return_ratio": 0.25,
        "return_rank": 1,
        "currency_rank": 1,
      },
      {
        "channel": "web",
        "item": "A",
        "return_ratio": 0.2,
        "return_rank": 1,
        "currency_rank": 1,
      },
      {
        "channel": "web",
        "item": "B",
        "return_ratio": 0.5,
        "return_rank": 2,
        "currency_rank": 2,
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  web = [
    {
      "item": "A",
      "return_ratio": 0.2,
      "currency_ratio": 0.3,
      "return_rank": 1,
      "currency_rank": 1,
    },
    {
      "item": "B",
      "return_ratio": 0.5,
      "currency_ratio": 0.6,
      "return_rank": 2,
      "currency_rank": 2,
    },
  ];
  catalog = [
    {
      "item": "A",
      "return_ratio": 0.3,
      "currency_ratio": 0.4,
      "return_rank": 1,
      "currency_rank": 1,
    },
  ];
  store = [
    {
      "item": "A",
      "return_ratio": 0.25,
      "currency_ratio": 0.35,
      "return_rank": 1,
      "currency_rank": 1,
    },
  ];
  tmp = [].concat(
    web.filter((w) => ((w.return_rank <= 10) || (w.currency_rank <= 10))).map(
      (w) => ({
        "channel": "web",
        "item": w.item,
        "return_ratio": w.return_ratio,
        "return_rank": w.return_rank,
        "currency_rank": w.currency_rank,
      })
    ),
    catalog.filter((c) => ((c.return_rank <= 10) || (c.currency_rank <= 10)))
      .map((c) => ({
        "channel": "catalog",
        "item": c.item,
        "return_ratio": c.return_ratio,
        "return_rank": c.return_rank,
        "currency_rank": c.currency_rank,
      })),
    store.filter((s) => ((s.return_rank <= 10) || (s.currency_rank <= 10))).map(
      (s) => ({
        "channel": "store",
        "item": s.item,
        "return_ratio": s.return_ratio,
        "return_rank": s.return_rank,
        "currency_rank": s.currency_rank,
      })
    ),
  );
  result = (() => {
    const _src = tmp;
    var _items = [];
    for (const r of _src) {
      _items.push({ r: r });
    }
    let _pairs = _items.map((it) => {
      const { r } = it;
      return {
        item: it,
        key: [r.channel, r.return_rank, r.currency_rank, r.item],
      };
    });
    _pairs.sort((a, b) => _cmp(a.key, b.key));
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const _it of _items) {
      const r = _it.r;
      _res.push(r);
    }
    return _res;
  })();
  console.log(_json(result));
  test_TPCDS_Q49_simplified();
}
function _cmp(a: unknown, b: unknown): number {
  if (Array.isArray(a) && Array.isArray(b)) {
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
      const c = _cmp(a[i], b[i]);
      if (c !== 0) return c;
    }
    return a.length - b.length;
  }
  if (typeof a === "number" && typeof b === "number") return a - b;
  if (typeof a === "string" && typeof b === "string") {
    const order: Record<string, number> = { store: 0, web: 1, catalog: 2 };
    if (order[a] !== undefined && order[b] !== undefined) {
      return order[a] - order[b];
    }
    return a < b ? -1 : (a > b ? 1 : 0);
  }
  return String(a) < String(b) ? -1 : (String(a) > String(b) ? 1 : 0);
}

function _equal(a: unknown, b: unknown): boolean {
  if (typeof a === "number" && typeof b === "number") {
    return Math.abs(a - b) < 1e-9;
  }
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
  return JSON.stringify(_sort(v));
}

main();
