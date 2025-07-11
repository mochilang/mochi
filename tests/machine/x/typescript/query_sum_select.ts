const nums = [
  1,
  2,
  3
];
const result = nums.filter((n) => (n > 1)).map((n) => (n.reduce((a,b)=>a+b,0)));
console.log(result);
