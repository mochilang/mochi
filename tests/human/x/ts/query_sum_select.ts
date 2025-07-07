const nums = [1,2,3];
const result = nums.filter(n => n > 1).reduce((acc, n) => acc + n, 0);
console.log(result);
