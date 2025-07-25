// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:12Z
// Source: tests/dataset/tpc-ds/q18.mochi

type CatalogSale = {
  cs_quantity: number;
  cs_list_price: number;
  cs_coupon_amt: number;
  cs_sales_price: number;
  cs_net_profit: number;
  cs_bill_cdemo_sk: number;
  cs_bill_customer_sk: number;
  cs_sold_date_sk: number;
  cs_item_sk: number;
};

type CustomerDemographics = {
  cd_demo_sk: number;
  cd_gender: string;
  cd_education_status: string;
  cd_dep_count: number;
};

type Customer = {
  c_customer_sk: number;
  c_current_cdemo_sk: number;
  c_current_addr_sk: number;
  c_birth_year: number;
  c_birth_month: number;
};

type CustomerAddress = {
  ca_address_sk: number;
  ca_country: string;
  ca_state: string;
  ca_county: string;
};

type DateDim = {
  d_date_sk: number;
  d_year: number;
};

type Item = {
  i_item_sk: number;
  i_item_id: string;
};

let catalog_sales: Record<string, any>[];
let customer: { [key: string]: number }[];
let customer_address: Record<string, any>[];
let customer_demographics: Record<string, any>[];
let date_dim: { [key: string]: number }[];
let item: Record<string, any>[];
let joined: Record<string, any>[];
let result: Record<string, any>[];

function test_TPCDS_Q18_averages(): void {
  if (
    !(_equal(result, [
      {
        "i_item_id": "I1",
        "ca_country": "US",
        "ca_state": "CA",
        "ca_county": "County1",
        "agg1": 1,
        "agg2": 10,
        "agg3": 1,
        "agg4": 9,
        "agg5": 2,
        "agg6": 1980,
        "agg7": 2,
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  catalog_sales = [
    {
      "cs_quantity": 1,
      "cs_list_price": 10,
      "cs_coupon_amt": 1,
      "cs_sales_price": 9,
      "cs_net_profit": 2,
      "cs_bill_cdemo_sk": 1,
      "cs_bill_customer_sk": 1,
      "cs_sold_date_sk": 1,
      "cs_item_sk": 1,
    },
  ];
  customer_demographics = [
    {
      "cd_demo_sk": 1,
      "cd_gender": "M",
      "cd_education_status": "College",
      "cd_dep_count": 2,
    },
    {
      "cd_demo_sk": 2,
      "cd_gender": "F",
      "cd_education_status": "College",
      "cd_dep_count": 2,
    },
  ];
  customer = [
    {
      "c_customer_sk": 1,
      "c_current_cdemo_sk": 2,
      "c_current_addr_sk": 1,
      "c_birth_year": 1980,
      "c_birth_month": 1,
    },
  ];
  customer_address = [
    {
      "ca_address_sk": 1,
      "ca_country": "US",
      "ca_state": "CA",
      "ca_county": "County1",
    },
  ];
  date_dim = [
    {
      "d_date_sk": 1,
      "d_year": 1999,
    },
  ];
  item = [
    {
      "i_item_sk": 1,
      "i_item_id": "I1",
    },
  ];
  joined = (() => {
    const _src = catalog_sales;
    const _res = [];
    for (const cs of _src) {
      for (const cd1 of customer_demographics) {
        if (
          !(((cs.cs_bill_cdemo_sk == cd1.cd_demo_sk) &&
            (cd1.cd_gender == "M")) && (cd1.cd_education_status == "College"))
        ) continue;
        for (const c of customer) {
          if (!(cs.cs_bill_customer_sk == c.c_customer_sk)) continue;
          for (const cd2 of customer_demographics) {
            if (!(c.c_current_cdemo_sk == cd2.cd_demo_sk)) continue;
            for (const ca of customer_address) {
              if (!(c.c_current_addr_sk == ca.ca_address_sk)) continue;
              for (const d of date_dim) {
                if (
                  !((cs.cs_sold_date_sk == d.d_date_sk) && (d.d_year == 1999))
                ) continue;
                for (const i of item) {
                  if (!(cs.cs_item_sk == i.i_item_sk)) continue;
                  _res.push({
                    "i_item_id": i.i_item_id,
                    "ca_country": ca.ca_country,
                    "ca_state": ca.ca_state,
                    "ca_county": ca.ca_county,
                    "q": cs.cs_quantity,
                    "lp": cs.cs_list_price,
                    "cp": cs.cs_coupon_amt,
                    "sp": cs.cs_sales_price,
                    "np": cs.cs_net_profit,
                    "by": c.c_birth_year,
                    "dep": cd1.cd_dep_count,
                  });
                }
              }
            }
          }
        }
      }
    }
    return _res;
  })();
  result = (() => {
    const _src = joined;
    const _map = new Map<string, any>();
    var _items = [];
    for (const j of _src) {
      const _key = {
        "i_item_id": j.i_item_id,
        "ca_country": j.ca_country,
        "ca_state": j.ca_state,
        "ca_county": j.ca_county,
      };
      const _ks = JSON.stringify(_key);
      let _g = _map.get(_ks);
      if (!_g) {
        _g = { key: _key, items: [] };
        _map.set(_ks, _g);
      }
      _g.items.push({ ...j, j: j });
    }
    let _groups = Array.from(_map.values());
    const _res = [];
    for (const g of _groups) {
      _res.push({
        "i_item_id": g.key.i_item_id,
        "ca_country": g.key.ca_country,
        "ca_state": g.key.ca_state,
        "ca_county": g.key.ca_county,
        "agg1": (g.items.map((x) =>
          x.q
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.q).length),
        "agg2": (g.items.map((x) =>
          x.lp
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.lp).length),
        "agg3": (g.items.map((x) =>
          x.cp
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.cp).length),
        "agg4": (g.items.map((x) =>
          x.sp
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.sp).length),
        "agg5": (g.items.map((x) =>
          x.np
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.np).length),
        "agg6": (g.items.map((x) =>
          x.by
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) => x.by).length),
        "agg7": (g.items.map((x) =>
          x.dep
        ).reduce((a, b) => a + Number(b), 0) / g.items.map((x) =>
          x.dep
        ).length),
      });
    }
    return _res;
  })();
  console.log(_json(result));
  test_TPCDS_Q18_averages();
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
