import 'dart:io';
import 'dart:convert';
import 'package:yaml/yaml.dart';

dynamic _load(String path, dynamic opts) {
  var fmt = 'csv';
  if (opts is Map && opts.containsKey('format')) fmt = opts['format'].toString();
  if (fmt == 'yaml') {
    var text = File(path).readAsStringSync();
    var data = loadYaml(text);
    if (data is List) return [for (var d in data) Map<String,dynamic>.from(d)];
    if (data is Map) return [Map<String,dynamic>.from(data)];
    return [];
  }
  var text = File(path).readAsStringSync();
  var data = jsonDecode(text);
  if (data is List) return data;
  if (data is Map) return [data];
  return [];
}

class Person {
  String name;
  int age;
  String email;
  Person(this.name, this.age, this.email);
}

var people = [for (var _it in (_load('tests/interpreter/valid/people.yaml', {'format': 'yaml'}))) Person((_it['name'] as String), (_it['age'] as int), (_it['email'] as String))];

var adults = (() {
  var _q0 = <dynamic>[];
  for (var p in people) {
    if (!(p.age >= 18)) continue;
    _q0.add({'name': p.name, 'email': p.email});
  }
  return _q0;
})();

void main() {
  var _iter1 = adults;
  for (var a in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print([a['name'], a['email']].join(' '));
  }
}
