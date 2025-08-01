// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:46:17Z
// Source: tests/dataset/tpc-ds/q89.mochi

let result: number;
let store_sales: { [key: string]: number }[];

function test_TPCDS_Q89_sample(): void {
  if (!(result == 89)) throw new Error("expect failed");
}

function main(): void {
  store_sales = [{ "price": 40 }, { "price": 30 }, { "price": 19 }];
  result = store_sales.map((s) => s.price).reduce((a, b) => a + b, 0);
  console.log(_json(result));
  test_TPCDS_Q89_sample();
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
