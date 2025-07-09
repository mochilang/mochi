void main() {
  var prefix = 'fore';
  var s1 = 'forest';
  print(
    ((s1 is String)
            ? s1.substring(0, prefix.length)
            : (s1 as List).sublist(0, prefix.length)) ==
        prefix,
  );
  var s2 = 'desert';
  print(
    ((s2 is String)
            ? s2.substring(0, prefix.length)
            : (s2 as List).sublist(0, prefix.length)) ==
        prefix,
  );
}
