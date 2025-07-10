import 'dart:io';
import 'package:yaml/yaml.dart';

void main() {
  var yamlStr = File('tests/interpreter/valid/people.yaml').readAsStringSync();
  var people = loadYaml(yamlStr) as List;
  var adults = people.where((p) => p['age'] >= 18).map((p) => {'name': p['name'], 'email': p['email']});
  for (var a in adults) {
    print('${a['name']} ${a['email']}');
  }
}
