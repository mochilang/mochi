let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
for (const n of numbers) {
  if (((n % 2) == 0)) {
    continue;
  }
  if ((n > 7)) {
    break;
  }
  console.log("odd number:", n);
}
