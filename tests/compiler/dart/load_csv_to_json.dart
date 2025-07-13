import 'dart:io';
import 'dart:convert';
import 'package:yaml/yaml.dart';

Map<String, Function> _structParsers = {};

class Person {
  String name;
  int age;
  Person({required this.name, required this.age});
  factory Person.fromJson(Map<String,dynamic> m) {
    return Person(name: m['name'] as String, age: m['age'] as int);
  }
}

List<Person> people = _load("../tests/interpreter/valid/people.csv", null);

void main() {
  _structParsers['Person'] = (m) => Person.fromJson(m);
  
  _save(people, "-", {"format": "json"});
}

List<Map<String,dynamic>> _load(String? path, Map<String,dynamic>? opts) {
    var format = (opts?['format'] ?? 'csv').toString();
    var header = opts?['header'] ?? true;
    var delim = (opts?['delimiter'] ?? ',').toString();
    if (delim.isEmpty) delim = ',';
    if (format == 'tsv') delim = '  ';
    String text;
    if (path == null || path == '' || path == '-') {
        var lines = <String>[];
        while (true) {
            var line = stdin.readLineSync();
            if (line == null) break;
            lines.add(line);
        }
        text = lines.join('\n');
    } else {
        text = File(path).readAsStringSync();
    }
    if (format == 'json') {
        var data = jsonDecode(text);
        if (data is List) return data.map((e) => Map<String,dynamic>.from(e as Map)).toList();
        if (data is Map) return [Map<String,dynamic>.from(data)];
        return <Map<String,dynamic>>[];
    }
    if (format == 'jsonl') {
        return text.trim().split(RegExp('\r?\n')).where((l) => l.isNotEmpty).map((l) => Map<String,dynamic>.from(jsonDecode(l))).toList();
    }
    if (format == 'yaml') {
        var data = loadYaml(text);
        if (data is YamlList) return data.map((e) => Map<String,dynamic>.from(e)).toList();
        if (data is YamlMap) return [Map<String,dynamic>.from(data)];
        return <Map<String,dynamic>>[];
    }
    if (format != 'csv') return <Map<String,dynamic>>[];
    var lines = text.trim().split(RegExp('\r?\n')).where((l) => l.isNotEmpty).toList();
    if (lines.isEmpty) return <Map<String,dynamic>>[];
    List<String> headers;
    if (header) {
        headers = lines[0].split(delim);
    } else {
        headers = List.generate(lines[0].split(delim).length, (i) => 'c$' + i.toString());
    }
    var start = header ? 1 : 0;
    var out = <Map<String,dynamic>>[];
    for (var i = start; i < lines.length; i++) {
        var parts = lines[i].split(delim);
        var row = <String,dynamic>{};
        for (var j = 0; j < headers.length; j++) {
            row[headers[j]] = j < parts.length ? parts[j] : '';
        }
        out.add(row);
    }
    return out;
}

void _save(List<Map<String,dynamic>> rows, String? path, Map<String,dynamic>? opts) {
    var format = (opts?['format'] ?? 'csv').toString();
    var header = opts?['header'] ?? false;
    var delim = (opts?['delimiter'] ?? ',').toString();
    if (delim.isEmpty) delim = ',';
    if (format == 'tsv') delim = '  ';
    String text;
    if (format == 'json') {
        text = jsonEncode(rows);
    } else if (format == 'jsonl') {
        text = rows.map((r) => jsonEncode(r)).join('\n') + '
';
    } else if (format == 'yaml') {
        var enc = YamlEncoder();
        text = enc.convert(rows.length == 1 ? rows[0] : rows);
    } else if (format == 'csv') {
        var headers = rows.isNotEmpty ? (rows[0].keys.toList()..sort()) : <String>[];
        var lines = <String>[];
        if (header) lines.add(headers.join(delim));
        for (var row in rows) {
            lines.add(headers.map((h) => row[h]?.toString() ?? '').join(delim));
        }
        text = lines.join('\n') + '
';
    } else {
        return;
    }
    if (path == null || path == '' || path == '-') {
        stdout.write(text);
    } else {
        File(path).writeAsStringSync(text);
    }
}
