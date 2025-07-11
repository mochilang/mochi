function twoSum(nums, target) {
  const n = nums.length;
  for (let i = 0; i < n; i++) {
    for (let j = (i + 1); j < n; j++) {
      if (((nums[i] + nums[j]) == target)) {
        return [i, j];
      }
    }
  }
  return [(-1), (-1)];
}
const result = twoSum([
  2,
  7,
  11,
  15
], 9);
console.log(result[0]);
console.log(result[1]);
