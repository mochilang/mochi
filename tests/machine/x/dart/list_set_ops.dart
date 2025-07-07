void main() {
  var union = {...[1, 2], ...[2, 3]};
  print(union.toList());

  var except = [1, 2, 3].where((x) => ![2].contains(x)).toList();
  print(except);

  var intersect = [1, 2, 3].where((x) => [2, 4].contains(x)).toList();
  print(intersect);

  var unionAll = []..addAll([1, 2])..addAll([2, 3]);
  print(unionAll.length);
}
