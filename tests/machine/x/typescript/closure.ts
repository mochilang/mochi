function makeAdder(n) {
  return (x) => (x + n);
}
const add10 = makeAdder(10);
console.log(add10(7));
