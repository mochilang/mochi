const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const _tmp1 = numbers;
for (const n of (Array.isArray(_tmp1) ? _tmp1 : Object.keys(_tmp1))) {
  if (((n % 2) == 0)) {
    continue;
  }
  if ((n > 7)) {
    break;
  }
  console.log("odd number:", n);
}
