// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/dataset/tpc-h/q10.mochi

let customer: Record<string, any>[];
let end_date: string;
let lineitem: Record<string, any>[];
let nation: Record<string, any>[];
let orders: Record<string, any>[];
let result: Record<string, any>[];
let start_date: string;

function test_Q10_returns_customer_revenue_from_returned_items(): void {
  if (
    !(_equal(result, [
      {
        "c_custkey": 1,
        "c_name": "Alice",
        "revenue": (1000 * 0.9),
        "c_acctbal": 100,
        "n_name": "BRAZIL",
        "c_address": "123 St",
        "c_phone": "123-456",
        "c_comment": "Loyal",
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  nation = [
    {
      "n_nationkey": 1,
      "n_name": "BRAZIL",
    },
  ];
  customer = [
    {
      "c_custkey": 1,
      "c_name": "Alice",
      "c_acctbal": 100,
      "c_nationkey": 1,
      "c_address": "123 St",
      "c_phone": "123-456",
      "c_comment": "Loyal",
    },
  ];
  orders = [
    {
      "o_orderkey": 1000,
      "o_custkey": 1,
      "o_orderdate": "1993-10-15",
    },
    {
      "o_orderkey": 2000,
      "o_custkey": 1,
      "o_orderdate": "1994-01-02",
    },
  ];
  lineitem = [
    {
      "l_orderkey": 1000,
      "l_returnflag": "R",
      "l_extendedprice": 1000,
      "l_discount": 0.1,
    },
    {
      "l_orderkey": 2000,
      "l_returnflag": "N",
      "l_extendedprice": 500,
      "l_discount": 0,
    },
  ];
  start_date = "1993-10-01";
  end_date = "1994-01-01";
  result = (() => {
    const _src = customer;
    const _map = new Map<string, any>();
    var _items = [];
    for (const c of _src) {
      for (const o of orders) {
        if (!(o.o_custkey == c.c_custkey)) continue;
        for (const l of lineitem) {
          if (!(l.l_orderkey == o.o_orderkey)) continue;
          for (const n of nation) {
            if (!(n.n_nationkey == c.c_nationkey)) continue;
            if (
              !(((o.o_orderdate >= start_date) && (o.o_orderdate < end_date)) &&
                (l.l_returnflag == "R"))
            ) continue;
            const _key = {
              "c_custkey": c.c_custkey,
              "c_name": c.c_name,
              "c_acctbal": c.c_acctbal,
              "c_address": c.c_address,
              "c_phone": c.c_phone,
              "c_comment": c.c_comment,
              "n_name": n.n_name,
            };
            const _ks = JSON.stringify(_key);
            let _g = _map.get(_ks);
            if (!_g) {
              _g = { key: _key, items: [] };
              _map.set(_ks, _g);
            }
            _g.items.push({ ...c, ...o, ...l, ...n, c: c, o: o, l: l, n: n });
          }
        }
      }
    }
    let _groups = Array.from(_map.values());
    var _items = _groups;
    let _pairs = _items.map((it) => {
      const g = it;
      return {
        item: it,
        key:
          (-g.items.map((x) => (x.l.l_extendedprice * (1 - x.l.l_discount)))
            .reduce((a, b) => a + Number(b), 0)),
      };
    });
    _pairs.sort((a, b) => a.key - b.key);
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const g of _items) {
      _res.push({
        "c_custkey": g.key.c_custkey,
        "c_name": g.key.c_name,
        "revenue": g.items.map(
          (x) => (x.l.l_extendedprice * (1 - x.l.l_discount))
        ).reduce((a, b) => a + Number(b), 0),
        "c_acctbal": g.key.c_acctbal,
        "n_name": g.key.n_name,
        "c_address": g.key.c_address,
        "c_phone": g.key.c_phone,
        "c_comment": g.key.c_comment,
      });
    }
    return _res;
  })();
  console.log(_json(result));
  test_Q10_returns_customer_revenue_from_returned_items();
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
