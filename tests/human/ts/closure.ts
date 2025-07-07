function makeAdder(n: number): (x: number) => number {
  return (x: number) => x + n;
}
const add10 = makeAdder(10);
console.log(add10(7));
