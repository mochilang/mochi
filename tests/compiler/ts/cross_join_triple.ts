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
  combos = [];
  for (const n of nums) {
    for (const l of letters) {
      for (const b of bools) {
        combos.push({
          "n": n,
          "l": l,
          "b": b,
        });
      }
    }
  }
  console.log("--- Cross Join of three lists ---");
  for (const c of combos) {
    console.log(`${c.n} ${c.l} ${c.b}`);
  }
}
main();
