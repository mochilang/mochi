function sumRec(n: number, acc: number): number {
  if (n === 0) {
    return acc;
  }
  return sumRec(n - 1, acc + n);
}
console.log(sumRec(10, 0));
