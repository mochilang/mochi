import 'dart:io';
import 'dart:convert';

dynamic _load(String path, dynamic opts) {
  var fmt = 'csv';
  if (opts is Map && opts.containsKey('format'))
    fmt = opts['format'].toString();
  if (fmt == 'yaml') {
    var text = File(path).readAsStringSync();
    var data = _parseYaml(text);
    return data;
  }
  var text = File(path).readAsStringSync();
  var data = jsonDecode(text);
  if (data is List) return data;
  if (data is Map) return [data];
  return [];
}

List<Map<String, dynamic>> _parseYaml(String text) {
  var rows = <Map<String, dynamic>>[];
  Map<String, dynamic>? cur;
  for (var line in LineSplitter.split(text)) {
    var t = line.trim();
    if (t.isEmpty) continue;
    if (t.startsWith('-')) {
      if (cur != null) rows.add(cur);
      cur = <String, dynamic>{};
      t = t.substring(1).trim();
      if (t.isEmpty) continue;
    }
    var idx = t.indexOf(':');
    if (idx <= 0) continue;
    var key = t.substring(0, idx).trim();
    var val = t.substring(idx + 1).trim();
    if ((val.startsWith("\"") && val.endsWith("\"")) ||
        (val.startsWith("'") && val.endsWith("'"))) {
      val = val.substring(1, val.length - 1);
    }
    var numVal = num.tryParse(val);
    cur?[key] = numVal ?? val;
  }
  if (cur != null) rows.add(cur);
  return rows;
}

class Person {
  String name;
  int age;
  String email;
  Person(this.name, this.age, this.email);
}

var people = [
  for (var _it in (_load('tests/interpreter/valid/people.yaml', {
    'format': 'yaml',
  })))
    Person(
      (_it['name'] as String),
      (_it['age'] as int),
      (_it['email'] as String),
    ),
];

var adults = (() {
  var _q0 = <dynamic>[];
  for (var p in people) {
    if (!(p.age >= 18)) continue;
    _q0.add({'name': p.name, 'email': p.email});
  }
  return _q0;
})();

void main() {
  for (var a in adults) {
    print([a['name'], a['email']].join(' '));
  }
}
