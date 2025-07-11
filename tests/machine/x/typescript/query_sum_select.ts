const nums = [
  1,
  2,
  3
];
const result = nums.filter((n) => (n > 1)).reduce((a,b)=>a+b,0);
console.log(result);
