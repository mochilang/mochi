const nations = [
  { id: 1, name: "A" },
  { id: 2, name: "B" },
];
const suppliers = [
  { id: 1, nation: 1 },
  { id: 2, nation: 2 },
];
const partsupp = [
  { part: 100, supplier: 1, cost: 10.0, qty: 2 },
  { part: 100, supplier: 2, cost: 20.0, qty: 1 },
  { part: 200, supplier: 1, cost: 5.0, qty: 3 },
];
const filtered = partsupp
  .map((ps) => {
    const s = suppliers.find((sp) => sp.id === ps.supplier);
    const n = s && nations.find((na) => na.id === s.nation);
    if (n && n.name === "A" && s) {
      return { part: ps.part, value: ps.cost * ps.qty };
    }
    return undefined;
  })
  .filter((x): x is { part: number; value: number } => x !== undefined);
const grouped: Record<number, number> = {};
for (const x of filtered) {
  grouped[x.part] = (grouped[x.part] || 0) + x.value;
}
const result = Object.entries(grouped).map(([part, total]) => ({
  part: Number(part),
  total,
}));
console.log(result);
