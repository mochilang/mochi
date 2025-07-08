void main() {
  print({...[1, 2], ...[2, 3]}.toList());
  print(List.from([1, 2, 3])..removeWhere((x) => [2].contains(x)));
  print([1, 2, 3].where((x) => [2, 4].contains(x)).toList());
  print(List.from([1, 2])..addAll([2, 3]).length);
}
