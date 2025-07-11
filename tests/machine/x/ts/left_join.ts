// Generated by Mochi TypeScript compiler

let customers: any[];
let orders: any[];
let result: any[];

function main(): void {
  customers = [
    {
      "id": 1,
      "name": "Alice",
    },
    {
      "id": 2,
      "name": "Bob",
    },
  ];
  orders = [
    {
      "id": 100,
      "customerId": 1,
      "total": 250,
    },
    {
      "id": 101,
      "customerId": 3,
      "total": 80,
    },
  ];
  result = (() => {
    const _src = orders;
    const _join = customers;
    const _res = [];
    for (const o of _src) {
      let _m = false;
      for (const c of _join) {
        if (!(o.customerId == c.id)) continue;
        _m = true;
        _res.push({
          "orderId": o.id,
          "customer": c,
          "total": o.total,
        });
      }
      if (!_m) {
        const c = null;
        _res.push({
          "orderId": o.id,
          "customer": c,
          "total": o.total,
        });
      }
    }
    return _res;
  })();
  console.log("--- Left Join ---");
  for (const entry of result) {
    console.log(
      "Order",
      entry.orderId,
      "customer",
      JSON.stringify(entry.customer),
      "total",
      entry.total,
    );
  }
}
main();
