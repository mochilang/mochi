console.log(Array.from(new Set([...[1, 2], ...[2, 3]])));
console.log([
  1,
  2,
  3
].filter(x => ![2].includes(x)));
console.log([
  1,
  2,
  3
].filter(x => [2, 4].includes(x)));
console.log([1, 2].concat([2, 3]).length);
