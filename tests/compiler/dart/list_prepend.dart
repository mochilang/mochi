List<List<int>> prepend(List<int> level, List<List<int>> result) {
  result = ([level] + result);
  return result;
}

void main() {
  print(prepend([1, 2], [[3], [4]]));
}
