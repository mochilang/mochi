void main() {
  var matrix = [
    [1, 2],
    [3, 4],
  ];
  (matrix[1] as Map)[0] = 5;
  print(matrix[1][0]);
}
