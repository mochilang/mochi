// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:17Z
// Source: tests/dataset/tpc-ds/q92.mochi

type WebSale = {
  ws_item_sk: number;
  ws_sold_date_sk: number;
  ws_ext_discount_amt: number;
};

let avg_amt: number;
let date_dim: Record<string, any>[];
let item: { [key: string]: number }[];
let result: number;
let sum_amt: number;
let web_sales: Record<string, any>[];

function test_TPCDS_Q92_threshold(): void {
  if (!(result == 4)) throw new Error("expect failed");
}

function main(): void {
  web_sales = [
    {
      "ws_item_sk": 1,
      "ws_sold_date_sk": 1,
      "ws_ext_discount_amt": 1,
    },
    {
      "ws_item_sk": 1,
      "ws_sold_date_sk": 1,
      "ws_ext_discount_amt": 1,
    },
    {
      "ws_item_sk": 1,
      "ws_sold_date_sk": 1,
      "ws_ext_discount_amt": 2,
    },
  ];
  item = [
    {
      "i_item_sk": 1,
      "i_manufact_id": 1,
    },
  ];
  date_dim = [
    {
      "d_date_sk": 1,
      "d_date": "2000-01-02",
    },
  ];
  sum_amt = web_sales.map((ws) => ws.ws_ext_discount_amt).reduce(
    (a, b) => a + Number(b),
    0,
  );
  avg_amt =
    web_sales.map((ws) => ws.ws_ext_discount_amt).reduce(
      (a, b) => a + Number(b),
      0,
    ) / web_sales.map((ws) => ws.ws_ext_discount_amt).length;
  result = (sum_amt > (avg_amt * 1.3)) ? sum_amt : 0;
  console.log(_json(result));
  test_TPCDS_Q92_threshold();
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
