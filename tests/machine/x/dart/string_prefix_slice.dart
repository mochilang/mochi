var prefix = 'fore';

var s1 = 'forest';

var s2 = 'desert';

void main() {
  print(
    ((s1 is String)
            ? s1.substring(0, prefix.length)
            : (s1 as List).sublist(0, prefix.length)) ==
        prefix,
  );
  print(
    ((s2 is String)
            ? s2.substring(0, prefix.length)
            : (s2 as List).sublist(0, prefix.length)) ==
        prefix,
  );
}
