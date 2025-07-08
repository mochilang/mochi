function makeAdder(n) {
  return (x) => (x + n);
}
let add10 = makeAdder(10);
console.log(add10(7));
