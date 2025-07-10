var people = [
  {'name': 'Alice', 'age': 30},
  {'name': 'Bob', 'age': 15},
  {'name': 'Charlie', 'age': 65},
  {'name': 'Diana', 'age': 45},
];

var adults = (() {
  var _q0 = <dynamic>[];
  for (var person in people) {
    if (!((person['age'] as num) >= 18)) continue;
    _q0.add({
      'name': person['name'],
      'age': person['age'],
      'is_senior': (person['age'] as num) >= 60,
    });
  }
  return _q0;
})();

void main() {
  print('--- Adults ---');
  var _iter1 = adults;
  for (var person
      in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print(
      [
        person['name'],
        'is',
        person['age'],
        (person['is_senior'] ? ' (senior)' : ''),
      ].join(' '),
    );
  }
}
