var nums = [3, 1, 4];

void main() {
  print(nums.reduce((a, b) => a < b ? a : b));
  print(nums.reduce((a, b) => a > b ? a : b));
}
