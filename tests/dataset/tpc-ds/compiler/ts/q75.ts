// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:16Z
// Source: tests/dataset/tpc-ds/q75.mochi

let all_sales: Record<string, any>[];
let catalog_sales: Record<string, any>[];
let curr_yr: any;
let date_dim: { [key: string]: number }[];
let item: Record<string, any>[];
let prev_yr: any;
let result: Record<string, any>[];
let sales_detail: any[];
let store_sales: Record<string, any>[];
let web_sales: Record<string, any>[];

function test_TPCDS_Q75_simplified(): void {
  if (
    !(_equal(result, [
      {
        "prev_year": 2000,
        "year": 2001,
        "i_brand_id": 1,
        "i_class_id": 2,
        "i_category_id": 3,
        "i_manufact_id": 4,
        "prev_yr_cnt": 100,
        "curr_yr_cnt": 80,
        "sales_cnt_diff": (-20),
        "sales_amt_diff": (-200),
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  date_dim = [
    {
      "d_date_sk": 1,
      "d_year": 2000,
    },
    {
      "d_date_sk": 2,
      "d_year": 2001,
    },
  ];
  store_sales = [
    {
      "ss_item_sk": 1,
      "ss_quantity": 50,
      "ss_sales_price": 500,
      "ss_sold_date_sk": 1,
    },
    {
      "ss_item_sk": 1,
      "ss_quantity": 40,
      "ss_sales_price": 400,
      "ss_sold_date_sk": 2,
    },
  ];
  web_sales = [
    {
      "ws_item_sk": 1,
      "ws_quantity": 30,
      "ws_sales_price": 300,
      "ws_sold_date_sk": 1,
    },
    {
      "ws_item_sk": 1,
      "ws_quantity": 25,
      "ws_sales_price": 250,
      "ws_sold_date_sk": 2,
    },
  ];
  catalog_sales = [
    {
      "cs_item_sk": 1,
      "cs_quantity": 20,
      "cs_sales_price": 200,
      "cs_sold_date_sk": 1,
    },
    {
      "cs_item_sk": 1,
      "cs_quantity": 15,
      "cs_sales_price": 150,
      "cs_sold_date_sk": 2,
    },
  ];
  item = [
    {
      "i_item_sk": 1,
      "i_brand_id": 1,
      "i_class_id": 2,
      "i_category_id": 3,
      "i_manufact_id": 4,
      "i_category": "Electronics",
    },
  ];
  sales_detail = [].concat(
    (() => {
      const _src = store_sales;
      const _join = date_dim;
      const _pairs = _hashJoin(_src, _join, (ss) =>
        ss.ss_sold_date_sk, (d) =>
        d.d_date_sk);
      const _res = [];
      for (const _p of _pairs) {
        const ss = _p[0];
        const d = _p[1];
        _res.push({
          "d_year": d.d_year,
          "i_item_sk": ss.ss_item_sk,
          "quantity": ss.ss_quantity,
          "amount": ss.ss_sales_price,
        });
      }
      return _res;
    })(),
    (() => {
      const _src = web_sales;
      const _join = date_dim;
      const _pairs = _hashJoin(_src, _join, (ws) =>
        ws.ws_sold_date_sk, (d) =>
        d.d_date_sk);
      const _res = [];
      for (const _p of _pairs) {
        const ws = _p[0];
        const d = _p[1];
        _res.push({
          "d_year": d.d_year,
          "i_item_sk": ws.ws_item_sk,
          "quantity": ws.ws_quantity,
          "amount": ws.ws_sales_price,
        });
      }
      return _res;
    })(),
    (() => {
      const _src = catalog_sales;
      const _join = date_dim;
      const _pairs = _hashJoin(_src, _join, (cs) =>
        cs.cs_sold_date_sk, (d) =>
        d.d_date_sk);
      const _res = [];
      for (const _p of _pairs) {
        const cs = _p[0];
        const d = _p[1];
        _res.push({
          "d_year": d.d_year,
          "i_item_sk": cs.cs_item_sk,
          "quantity": cs.cs_quantity,
          "amount": cs.cs_sales_price,
        });
      }
      return _res;
    })(),
  );
  all_sales = (() => {
    const _src = sales_detail;
    const _map = new Map<string, any>();
    var _items = [];
    for (const sd of _src) {
      for (const i of item) {
        if (!(i.i_item_sk == sd.i_item_sk)) continue;
        if (!(i.i_category == "Electronics")) continue;
        const _key = {
          "year": sd.d_year,
          "brand_id": i.i_brand_id,
          "class_id": i.i_class_id,
          "category_id": i.i_category_id,
          "manuf_id": i.i_manufact_id,
        };
        const _ks = JSON.stringify(_key);
        let _g = _map.get(_ks);
        if (!_g) {
          _g = { key: _key, items: [] };
          _map.set(_ks, _g);
        }
        _g.items.push({ ...sd, ...i, sd: sd, i: i });
      }
    }
    let _groups = Array.from(_map.values());
    const _res = [];
    for (const g of _groups) {
      _res.push({
        "d_year": g.key.year,
        "i_brand_id": g.key.brand_id,
        "i_class_id": g.key.class_id,
        "i_category_id": g.key.category_id,
        "i_manufact_id": g.key.manuf_id,
        "sales_cnt": g.items.map((x) => x.sd.quantity).reduce(
          (a, b) => a + Number(b),
          0,
        ),
        "sales_amt": g.items.map((x) => x.sd.amount).reduce(
          (a, b) => a + Number(b),
          0,
        ),
      });
    }
    return _res;
  })();
  prev_yr = (all_sales.filter((a) => (a.d_year == 2000)).map((a) => a))[0];
  curr_yr = (all_sales.filter((a) => (a.d_year == 2001)).map((a) => a))[0];
  result = ((curr_yr.sales_cnt / prev_yr.sales_cnt) < 0.9)
    ? [
      {
        "prev_year": prev_yr.d_year,
        "year": curr_yr.d_year,
        "i_brand_id": curr_yr.i_brand_id,
        "i_class_id": curr_yr.i_class_id,
        "i_category_id": curr_yr.i_category_id,
        "i_manufact_id": curr_yr.i_manufact_id,
        "prev_yr_cnt": prev_yr.sales_cnt,
        "curr_yr_cnt": curr_yr.sales_cnt,
        "sales_cnt_diff": (curr_yr.sales_cnt - prev_yr.sales_cnt),
        "sales_amt_diff": (curr_yr.sales_amt - prev_yr.sales_amt),
      },
    ]
    : [];
  console.log(_json(result));
  test_TPCDS_Q75_simplified();
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

function _hashJoin(
  left: any[],
  right: any[],
  lk: (v: any) => any,
  rk: (v: any) => any,
): any[] {
  const idx = new Map<any, any[]>();
  for (const r of right) {
    const k = rk(r);
    const arr = idx.get(k);
    if (arr) arr.push(r);
    else idx.set(k, [r]);
  }
  const out: any[] = [];
  for (const l of left) {
    const arr = idx.get(lk(l));
    if (!arr) continue;
    for (const r of arr) out.push([l, r]);
  }
  return out;
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
