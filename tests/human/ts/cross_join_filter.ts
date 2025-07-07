const nums = [1, 2, 3];
const letters = ["A", "B"];

const pairs = [] as {n:number; l:string;}[];
for (const n of nums) {
  for (const l of letters) {
    if (n % 2 === 0) {
      pairs.push({ n, l });
    }
  }
}

console.log("--- Even pairs ---");
for (const p of pairs) {
  console.log(p.n, p.l);
}
