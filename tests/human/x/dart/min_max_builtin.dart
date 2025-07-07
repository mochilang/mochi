void main() {
  var nums = [3, 1, 4];
  print(nums.reduce((a, b) => a < b ? a : b));
  print(nums.reduce((a, b) => a > b ? a : b));
}
