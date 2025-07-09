List<int> twoSum(List<int> nums, int target) {
  var n = nums.length;
  for (var i = 0; i < n; i++) {
    for (var j = (i as num) + 1; j < n; j++) {
      if (nums[i] + nums[j] == target) {
        return [i, j];
      }
    }
  }
  return [-1, -1];
}

void main() {
  var result = twoSum([2, 7, 11, 15], 9);
  print(result[0]);
  print(result[1]);
}
