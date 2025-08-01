// Generated by Mochi v0.10.36 on 2025-07-22 17:16:19 GMT+7

export interface Item { n: number; v: string }
const items: Item[] = [{n: 1, v: "a"}, {n: 1, v: "b"}, {n: 2, v: "c"}];
const result: any[] = (() => {
  const result = []
  for (const i of items) {
    result.push({k: i.n, v: i.v})
  }
  result.sort((a, b) => a.k.n < b.k.n ? -1 : a.k.n > b.k.n ? 1 : 0)
  const out = result.map(r => r.v)
  return out
})();
console.log("[" + (result).join(", ") + "]");
