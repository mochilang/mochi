void main() {
  var people = [
    {'name': 'Alice', 'age': 30},
    {'name': 'Bob', 'age': 15},
    {'name': 'Charlie', 'age': 65},
    {'name': 'Diana', 'age': 45}
  ];
  var adults = people
      .where((p) => p['age'] >= 18)
      .map((p) => {
            'name': p['name'],
            'age': p['age'],
            'is_senior': p['age'] >= 60
          })
      .toList();
  print('--- Adults ---');
  for (var p in adults) {
    var senior = p['is_senior'] ? ' (senior)' : '';
    print('${p['name']} is ${p['age']}$senior');
  }
}
