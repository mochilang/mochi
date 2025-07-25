// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:34:43Z
bool sameDigits(int n, int b) {
  var f = n % b;
  n = int.parse((n ~/ b));
  while (n > 0) {
    if (n % b != f) {
      return false;
    }
    n = int.parse((n ~/ b));
  }
  return true;
}

bool isBrazilian(int n) {
  if (n < 7) {
    return false;
  }
  if (n % 2 == 0 && n >= 8) {
    return true;
  }
  var b = 2;
  while ((b as num) < n - 1) {
    if (sameDigits(n, b)) {
      return true;
    }
    b = (b as num) + 1;
  }
  return false;
}

bool isPrime(int n) {
  if (n < 2) {
    return false;
  }
  if (n % 2 == 0) {
    return n == 2;
  }
  if (n % 3 == 0) {
    return n == 3;
  }
  var d = 5;
  while (((d as num) * (d as num) as num) <= n) {
    if (n % (d as num) == 0) {
      return false;
    }
    d = (d as num) + 2;
    if (n % (d as num) == 0) {
      return false;
    }
    d = (d as num) + 4;
  }
  return true;
}

void _main() {
  var kinds = [' ', ' odd ', ' prime '];
  var _iter0 = kinds;
  for (var kind in (_iter0 is Map ? (_iter0 as Map).keys : _iter0) as Iterable) {
    print('First 20' + kind + 'Brazilian numbers:');
    num c = 0;
    var n = 7;
    while (true) {
      if (isBrazilian(n)) {
        print(n.toString() + ' ');
        c = (c as num) + 1;
        if (c == 20) {
          print('\n');
          break;
        }
      }
      if (kind == ' ') {
        n = (n as num) + 1;
      }
      else 
      if (kind == ' odd ') {
        n = (n as num) + 2;
      }
      else {
        while (true) {
          n = (n as num) + 2;
          if (isPrime(n)) {
            break;
          }
        }
      }
    }
  }
  var n = 7;
  num c = 0;
  while ((c as num) < 100000) {
    if (isBrazilian(n)) {
      c = (c as num) + 1;
    }
    n = (n as num) + 1;
  }
  print('The 100,000th Brazilian number: ' + (n as num) - 1.toString());
}

void main() {
  _main();
}
