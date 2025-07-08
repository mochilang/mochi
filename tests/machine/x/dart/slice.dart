void main() {
  print((([1, 2, 3] is String) ? [1, 2, 3].substring(1, 3) : ([1, 2, 3] as List).sublist(1, 3)));
  print((([1, 2, 3] is String) ? [1, 2, 3].substring(0, 2) : ([1, 2, 3] as List).sublist(0, 2)));
  print((('hello' is String) ? 'hello'.substring(1, 4) : ('hello' as List).sublist(1, 4)));
}
