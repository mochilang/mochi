// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/left_join_multi.mochi

let customers: Record<string, any>[];
let items: Record<string, any>[];
let orders: { [key: string]: number }[];
let result: Record<string, any>[];

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
    },
    {
      "id": 101,
      "customerId": 2,
    },
  ];
  items = [
    {
      "orderId": 100,
      "sku": "a",
    },
  ];
  result = (() => {
    const _src = orders;
    return _query(_src, [
      { items: customers, on: (o, c) => (o.customerId == c.id) },
      { items: items, on: (o, c, i) => (o.id == i.orderId), left: true },
    ], {
      select: (o, c, i) => ({
        "orderId": o.id,
        "name": c.name,
        "item": i,
      }),
    });
  })();
  console.log("--- Left Join Multi ---");
  for (const r of result) {
    console.log(
      [r.orderId, r.name, r.item].map((a) => {
        if (Array.isArray(a)) return a.join(" ");
        if (typeof a === "boolean") return a ? "1" : "0";
        return String(a);
      }).join(" ").trimEnd(),
    );
  }
}
function _query(src: any[], joins: any[], opts: any): any {
  let items = src.map((v) => [v]);
  for (const j of joins) {
    const joined: any[] = [];
    if (j.right && j.left) {
      const matched: boolean[] = new Array(j.items.length).fill(false);
      for (const left of items) {
        let m = false;
        for (let ri = 0; ri < j.items.length; ri++) {
          const right = j.items[ri];
          let keep = true;
          if (right === null) {
            keep = false;
          } else if (j.on) keep = j.on(...left, right);
          if (!keep) continue;
          m = true;
          matched[ri] = true;
          joined.push([...left, right]);
        }
        if (!m) joined.push([...left, null]);
      }
      for (let ri = 0; ri < j.items.length; ri++) {
        if (!matched[ri]) {
          const undef = Array(items[0]?.length || 0).fill(null);
          joined.push([...undef, j.items[ri]]);
        }
      }
    } else if (j.right) {
      for (const right of j.items) {
        let m = false;
        for (const left of items) {
          let keep = true;
          if (right === null) {
            keep = false;
          } else if (j.on) keep = j.on(...left, right);
          if (!keep) continue;
          m = true;
          joined.push([...left, right]);
        }
        if (!m) {
          const undef = Array(items[0]?.length || 0).fill(null);
          joined.push([...undef, right]);
        }
      }
    } else {
      for (const left of items) {
        let m = false;
        for (const right of j.items) {
          let keep = true;
          if (right === null) {
            keep = false;
          } else if (j.on) keep = j.on(...left, right);
          if (!keep) continue;
          m = true;
          joined.push([...left, right]);
        }
        if (j.left && !m) joined.push([...left, null]);
      }
    }
    items = joined;
  }
  if (opts.where) items = items.filter((r) => opts.where(...r));
  if (opts.sortKey) {
    let pairs = items.map((it) => ({ item: it, key: opts.sortKey(...it) }));
    pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    items = pairs.map((p) => p.item);
  }
  if (opts.skip !== undefined) {
    const n = opts.skip;
    items = n < items.length ? items.slice(n) : [];
  }
  if (opts.take !== undefined) {
    const n = opts.take;
    if (n < items.length) items = items.slice(0, n);
  }
  const res = [];
  for (const r of items) res.push(opts.select(...r));
  return res;
}

main();
