void main() {
  var nums = [1, 2, 3];
  var average = nums.reduce((a, b) => a + b) / nums.length;
  print(average);
}
