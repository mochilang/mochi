// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:13Z
// Source: tests/dataset/tpc-ds/q26.mochi

type CatalogSale = {
  cs_sold_date_sk: number;
  cs_item_sk: number;
  cs_bill_cdemo_sk: number;
  cs_promo_sk: number;
  cs_quantity: number;
  cs_list_price: number;
  cs_coupon_amt: number;
  cs_sales_price: number;
};

type CustomerDemo = {
  cd_demo_sk: number;
  cd_gender: string;
  cd_marital_status: string;
  cd_education_status: string;
};

type DateDim = {
  d_date_sk: number;
  d_year: number;
};

type Item = {
  i_item_sk: number;
  i_item_id: string;
};

type Promotion = {
  p_promo_sk: number;
  p_channel_email: string;
  p_channel_event: string;
};

let catalog_sales: Record<string, any>[];
let customer_demographics: Record<string, any>[];
let date_dim: { [key: string]: number }[];
let item: Record<string, any>[];
let promotion: Record<string, any>[];
let result: Record<string, any>[];

function test_TPCDS_Q26_demographic_averages(): void {
  if (
    !(_equal(result, [
      {
        "i_item_id": "ITEM1",
        "agg1": 10,
        "agg2": 100,
        "agg3": 5,
        "agg4": 95,
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  catalog_sales = [
    {
      "cs_sold_date_sk": 1,
      "cs_item_sk": 1,
      "cs_bill_cdemo_sk": 1,
      "cs_promo_sk": 1,
      "cs_quantity": 10,
      "cs_list_price": 100,
      "cs_coupon_amt": 5,
      "cs_sales_price": 95,
    },
    {
      "cs_sold_date_sk": 1,
      "cs_item_sk": 2,
      "cs_bill_cdemo_sk": 2,
      "cs_promo_sk": 2,
      "cs_quantity": 5,
      "cs_list_price": 50,
      "cs_coupon_amt": 2,
      "cs_sales_price": 48,
    },
  ];
  customer_demographics = [
    {
      "cd_demo_sk": 1,
      "cd_gender": "M",
      "cd_marital_status": "S",
      "cd_education_status": "College",
    },
    {
      "cd_demo_sk": 2,
      "cd_gender": "F",
      "cd_marital_status": "M",
      "cd_education_status": "High School",
    },
  ];
  date_dim = [
    {
      "d_date_sk": 1,
      "d_year": 2000,
    },
  ];
  item = [
    {
      "i_item_sk": 1,
      "i_item_id": "ITEM1",
    },
    {
      "i_item_sk": 2,
      "i_item_id": "ITEM2",
    },
  ];
  promotion = [
    {
      "p_promo_sk": 1,
      "p_channel_email": "N",
      "p_channel_event": "Y",
    },
    {
      "p_promo_sk": 2,
      "p_channel_email": "Y",
      "p_channel_event": "N",
    },
  ];
  result = (() => {
    const _src = catalog_sales;
    const _map = new Map<string, any>();
    var _items = [];
    for (const cs of _src) {
      for (const cd of customer_demographics) {
        if (!(cs.cs_bill_cdemo_sk == cd.cd_demo_sk)) continue;
        for (const d of date_dim) {
          if (!(cs.cs_sold_date_sk == d.d_date_sk)) continue;
          for (const i of item) {
            if (!(cs.cs_item_sk == i.i_item_sk)) continue;
            for (const p of promotion) {
              if (!(cs.cs_promo_sk == p.p_promo_sk)) continue;
              if (
                !(((((cd.cd_gender == "M") && (cd.cd_marital_status == "S")) &&
                  (cd.cd_education_status == "College")) &&
                  ((p.p_channel_email == "N") || (p.p_channel_event == "N"))) &&
                  (d.d_year == 2000))
              ) continue;
              const _key = i.i_item_id;
              const _ks = JSON.stringify(_key);
              let _g = _map.get(_ks);
              if (!_g) {
                _g = { key: _key, items: [] };
                _map.set(_ks, _g);
              }
              _g.items.push({
                ...cs,
                ...cd,
                ...d,
                ...i,
                ...p,
                cs: cs,
                cd: cd,
                d: d,
                i: i,
                p: p,
              });
            }
          }
        }
      }
    }
    let _groups = Array.from(_map.values());
    const _res = [];
    for (const g of _groups) {
      _res.push({
        "i_item_id": g.key,
        "agg1": (g.items.map((x) =>
          x.cs_quantity
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) =>
          x.cs_quantity
        ).length),
        "agg2": (g.items.map((x) =>
          x.cs_list_price
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) =>
          x.cs_list_price
        ).length),
        "agg3": (g.items.map((x) =>
          x.cs_coupon_amt
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) =>
          x.cs_coupon_amt
        ).length),
        "agg4": (g.items.map((x) =>
          x.cs_sales_price
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) =>
          x.cs_sales_price
        ).length),
      });
    }
    return _res;
  })();
  console.log(_json(result));
  test_TPCDS_Q26_demographic_averages();
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
