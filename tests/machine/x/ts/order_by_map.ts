// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/order_by_map.mochi

let data: { [key: string]: number }[];
let sorted: { [key: string]: number }[];

function main(): void {
  data = [
    {
      "a": 1,
      "b": 2,
    },
    {
      "a": 1,
      "b": 1,
    },
    {
      "a": 0,
      "b": 5,
    },
  ];
  sorted = (() => {
    const _src = data;
    var _items = [];
    for (const x of _src) {
      _items.push({ x: x });
    }
    let _pairs = _items.map((it) => {
      const { x } = it;
      return {
        item: it,
        key: {
          "a": x.a,
          "b": x.b,
        },
      };
    });
    _pairs.sort((a, b) => _cmp(a.key, b.key));
    _items = _pairs.map((p) => p.item);
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const _it of _items) {
      const x = _it.x;
      _res.push(x);
    }
    return _res;
  })();
  console.log(sorted.join(" "));
}
function _cmp(a: unknown, b: unknown): number {
  if (Array.isArray(a) && Array.isArray(b)) {
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
      const c = _cmp(a[i], b[i]);
      if (c !== 0) return c;
    }
    return a.length - b.length;
  }
  if (typeof a === "number" && typeof b === "number") return a - b;
  if (typeof a === "string" && typeof b === "string") {
    const order: Record<string, number> = { store: 0, web: 1, catalog: 2 };
    if (order[a] !== undefined && order[b] !== undefined) {
      return order[a] - order[b];
    }
    return a < b ? -1 : (a > b ? 1 : 0);
  }
  return String(a) < String(b) ? -1 : (String(a) > String(b) ? 1 : 0);
}

main();
