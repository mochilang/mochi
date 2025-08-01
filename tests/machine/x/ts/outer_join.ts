// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/outer_join.mochi

let customers: Record<string, any>[];
let orders: { [key: string]: number }[];
let result: { [key: string]: { [key: string]: number } }[];

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
    {
      "id": 3,
      "name": "Charlie",
    },
    {
      "id": 4,
      "name": "Diana",
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
      "customerId": 2,
      "total": 125,
    },
    {
      "id": 102,
      "customerId": 1,
      "total": 300,
    },
    {
      "id": 103,
      "customerId": 5,
      "total": 80,
    },
  ];
  result = (() => {
    const _src = orders;
    const _join = customers;
    const _matched = new Array(_join.length).fill(false);
    const _res = [];
    for (const o of _src) {
      let _m = false;
      for (let _ri = 0; _ri < _join.length; _ri++) {
        const c = _join[_ri];
        if (!(o.customerId == c.id)) continue;
        _matched[_ri] = true;
        _m = true;
        _res.push({
          "order": o,
          "customer": c,
        });
      }
      if (!_m) {
        const c = null;
        _res.push({
          "order": o,
          "customer": c,
        });
      }
    }
    for (let _ri = 0; _ri < _join.length; _ri++) {
      if (!_matched[_ri]) {
        const o = null;
        const c = _join[_ri];
        _res.push({
          "order": o,
          "customer": c,
        });
      }
    }
    return _res;
  })();
  console.log("--- Outer Join using syntax ---");
  for (const row of result) {
    if (row.order) {
      if (row.customer) {
        console.log(
          [
            "Order",
            row.order.id,
            "by",
            row.customer.name,
            "- $",
            row.order.total,
          ].map((a) => {
            if (Array.isArray(a)) return a.join(" ");
            if (typeof a === "boolean") return a ? "1" : "0";
            return String(a);
          }).join(" ").trimEnd(),
        );
      } else {
        console.log(
          ["Order", row.order.id, "by", "Unknown", "- $", row.order.total].map(
            (a) => {
              if (Array.isArray(a)) return a.join(" ");
              if (typeof a === "boolean") return a ? "1" : "0";
              return String(a);
            },
          ).join(" ").trimEnd(),
        );
      }
    } else {
      console.log(
        ["Customer", row.customer.name, "has no orders"].map((a) => {
          if (Array.isArray(a)) return a.join(" ");
          if (typeof a === "boolean") return a ? "1" : "0";
          return String(a);
        }).join(" ").trimEnd(),
      );
    }
  }
}
main();
