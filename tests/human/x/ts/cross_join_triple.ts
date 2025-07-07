const nums = [1, 2];
const letters = ["A", "B"];
const bools = [true, false];
const combos = [] as Array<{ n: number; l: string; b: boolean }>;
for (const n of nums) {
  for (const l of letters) {
    for (const b of bools) {
      combos.push({ n, l, b });
    }
  }
}
console.log("--- Cross Join of three lists ---");
for (const c of combos) {
  console.log(c.n, c.l, c.b);
}
