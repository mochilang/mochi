// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:12Z
// Source: tests/dataset/tpc-ds/q11.mochi

type Customer = {
  c_customer_sk: number;
  c_customer_id: string;
  c_first_name: string;
  c_last_name: string;
};

type StoreSale = {
  ss_customer_sk: number;
  ss_sold_date_sk: number;
  ss_ext_list_price: number;
};

type WebSale = {
  ws_bill_customer_sk: number;
  ws_sold_date_sk: number;
  ws_ext_list_price: number;
};

let customer: Record<string, any>[];
let growth_ok: boolean;
let result: { [key: string]: string }[];
let ss98: number;
let ss99: number;
let store_sales: Record<string, any>[];
let web_sales: Record<string, any>[];
let ws98: number;
let ws99: number;

function test_TPCDS_Q11_growth(): void {
  if (
    !(_equal(result, [
      {
        "customer_id": "C1",
        "customer_first_name": "John",
        "customer_last_name": "Doe",
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  customer = [
    {
      "c_customer_sk": 1,
      "c_customer_id": "C1",
      "c_first_name": "John",
      "c_last_name": "Doe",
    },
  ];
  store_sales = [
    {
      "ss_customer_sk": 1,
      "ss_sold_date_sk": 1998,
      "ss_ext_list_price": 60,
    },
    {
      "ss_customer_sk": 1,
      "ss_sold_date_sk": 1999,
      "ss_ext_list_price": 90,
    },
  ];
  web_sales = [
    {
      "ws_bill_customer_sk": 1,
      "ws_sold_date_sk": 1998,
      "ws_ext_list_price": 50,
    },
    {
      "ws_bill_customer_sk": 1,
      "ws_sold_date_sk": 1999,
      "ws_ext_list_price": 150,
    },
  ];
  ss98 = store_sales.filter((ss) => (ss.ss_sold_date_sk == 1998)).map((ss) =>
    ss.ss_ext_list_price
  ).reduce((a, b) => a + Number(b), 0);
  ss99 = store_sales.filter((ss) => (ss.ss_sold_date_sk == 1999)).map((ss) =>
    ss.ss_ext_list_price
  ).reduce((a, b) => a + Number(b), 0);
  ws98 = web_sales.filter((ws) => (ws.ws_sold_date_sk == 1998)).map((ws) =>
    ws.ws_ext_list_price
  ).reduce((a, b) => a + Number(b), 0);
  ws99 = web_sales.filter((ws) => (ws.ws_sold_date_sk == 1999)).map((ws) =>
    ws.ws_ext_list_price
  ).reduce((a, b) => a + Number(b), 0);
  growth_ok = ((ws98 > 0) && (ss98 > 0)) && ((ws99 / ws98) > (ss99 / ss98));
  result = growth_ok
    ? [
      {
        "customer_id": "C1",
        "customer_first_name": "John",
        "customer_last_name": "Doe",
      },
    ]
    : [];
  console.log(_json(result));
  test_TPCDS_Q11_growth();
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
