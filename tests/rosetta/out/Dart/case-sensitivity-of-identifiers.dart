// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:35:02Z
void _main() {
  var pkg_dog = 'Salt';
  var Dog = 'Pepper';
  var pkg_DOG = 'Mustard';
  Map<String, bool> packageSees(String d1, String d2, String d3) {
    print('Package sees: ' + d1 + ' ' + d2 + ' ' + d3);
    return {'pkg_dog': true, 'Dog': true, 'pkg_DOG': true};
  }
  
  var d = packageSees(pkg_dog, Dog, pkg_DOG);
  print('There are ' + d.length.toString() + ' dogs.\n');
  var dog = 'Benjamin';
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  print('Main sees:   ' + dog + ' ' + Dog + ' ' + pkg_DOG);
  d['dog'] = true;
  d['Dog'] = true;
  d['pkg_DOG'] = true;
  print('There are ' + d.length.toString() + ' dogs.\n');
  Dog = 'Samba';
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  print('Main sees:   ' + dog + ' ' + Dog + ' ' + pkg_DOG);
  d['dog'] = true;
  d['Dog'] = true;
  d['pkg_DOG'] = true;
  print('There are ' + d.length.toString() + ' dogs.\n');
  var DOG = 'Bernie';
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  print('Main sees:   ' + dog + ' ' + Dog + ' ' + DOG);
  d['dog'] = true;
  d['Dog'] = true;
  d['pkg_DOG'] = true;
  d['DOG'] = true;
  print('There are ' + d.length.toString() + ' dogs.');
}

void main() {
  _main();
}
