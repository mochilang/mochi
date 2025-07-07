function outer(x: number): number {
  function inner(y: number): number {
    return x + y;
  }
  return inner(5);
}
console.log(outer(3));
