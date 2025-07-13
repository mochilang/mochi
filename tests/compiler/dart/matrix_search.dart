bool searchMatrix(List<List<int>> matrix, int target) {
  int m = matrix.length;
  if ((m == 0)) {
    return false;
  }
  int n = matrix[0].length;
  int left = 0;
  int right = ((m * n) - 1);
  while ((left <= right)) {
    int mid = (left + (((right - left)) ~/ 2));
    int row = (mid ~/ n);
    int col = (mid % n);
    int value = matrix[row][col];
    if ((value == target)) {
      return true;
    } else 
    if ((value < target)) {
      left = ((mid + 1)).toInt();
    } else {
      right = ((mid - 1)).toInt();
    }
  }
  return false;
}

void main() {
  print(searchMatrix([[1, 3, 5, 7], [10, 11, 16, 20], [23, 30, 34, 60]], 3));
  print(searchMatrix([[1, 3, 5, 7], [10, 11, 16, 20], [23, 30, 34, 60]], 13));
}
