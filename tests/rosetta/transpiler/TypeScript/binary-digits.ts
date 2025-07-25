// Generated by Mochi v0.10.40 on 2025-07-25 17:41:29 GMT+7

function toBin(n: number): string {
  if ((n == 0)) {
    return "0";
  }
  let bits: string = "";
  let x: number = n;
  while ((x > 0)) {
    bits = (String((x % 2)) + bits);
    x = Math.trunc(Math.trunc(x / 2));
  }
  return bits;
}
for (let i = 0; i < 16; i++) {
  console.log(toBin(i));
}
