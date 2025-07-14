let bools: boolean[];
let combos: Record<string, any>[];
let letters: string[];
let nums: number[];

function main(): void {
  nums = [
    1,
    2,
  ];
  letters = [
    "A",
    "B",
  ];
  bools = [
    true,
    false,
  ];
  const _src = nums;
  const _res = [];
  for (const n of _src) {
    for (const l of letters) {
      for (const b of bools) {
        _res.push({
          "n": n,
          "l": l,
          "b": b,
        });
      }
    }
  }
  combos = _res;
  console.log("--- Cross Join of three lists ---");
  for (const c of combos) {
    console.log(`${c.n} ${c.l} ${c.b}`);
  }
}
main();
