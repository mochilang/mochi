function add(a: number, b: number): number {
  return a + b;
}
const add5 = (b: number) => add(5, b);
console.log(add5(3));
