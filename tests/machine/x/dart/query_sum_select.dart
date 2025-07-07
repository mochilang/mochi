void main() {
  var nums = [1, 2, 3];
  var result = nums.where((n) => n > 1).fold(0, (a, b) => a + b);
  print(result);
}
