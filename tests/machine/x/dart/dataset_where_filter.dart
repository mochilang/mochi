void main() {
  var people = [{'name': 'Alice', 'age': 30}, {'name': 'Bob', 'age': 15}, {'name': 'Charlie', 'age': 65}, {'name': 'Diana', 'age': 45}];
  var adults = (() {
  var _q0 = <dynamic>[];
  for (var person in people) {
    if (!(person.age >= 18)) continue;
    _q0.add({'name': person.name, 'age': person.age, 'is_senior': person.age >= 60});
  }
  return _q0;
})();
  print('--- Adults ---');
  for (var person in adults) {
    print([person.name, 'is', person.age, (person.is_senior ? ' (senior)' : '')].join(' '));
  }
}
