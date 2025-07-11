// Generated by Mochi TypeScript compiler

let big: any[];
let people: any[];

function main(): void {
  people = [
    {
      "name": "Alice",
      "city": "Paris",
    },
    {
      "name": "Bob",
      "city": "Hanoi",
    },
    {
      "name": "Charlie",
      "city": "Paris",
    },
    {
      "name": "Diana",
      "city": "Hanoi",
    },
    {
      "name": "Eve",
      "city": "Paris",
    },
    {
      "name": "Frank",
      "city": "Hanoi",
    },
    {
      "name": "George",
      "city": "Paris",
    },
  ];
  big = (() => {
    const _src = people;
    const _map = new Map<string, any>();
    const _order: string[] = [];
    var _items = [];
    for (const p of _src) {
      const _key = p.city;
      const _ks = JSON.stringify(_key);
      let _g = _map.get(_ks);
      if (!_g) {
        _g = { key: _key, items: [] };
        _map.set(_ks, _g);
        _order.push(_ks);
      }
      _g.items.push({ ...p, p: p });
    }
    let _groups = _order.map((k) => _map.get(k)!);
    _groups = _groups.filter((g) => (_count(g) >= 4));
    const _res = [];
    for (const g of _groups) {
      _res.push({
        "city": g.key,
        "num": _count(g),
      });
    }
    return _res;
  })();
  console.log(_json(big));
}
function _count(v: any): number {
  if (Array.isArray(v)) return v.length;
  if (v && typeof v === "object") {
    if (Array.isArray((v as any).items)) return (v as any).items.length;
    if (Array.isArray((v as any).Items)) return (v as any).Items.length;
  }
  return 0;
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
