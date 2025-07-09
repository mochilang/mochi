function outer(x) {
  function inner(y) {
    return (x + y);
  }
  return inner(5);
}
console.log(outer(3));
