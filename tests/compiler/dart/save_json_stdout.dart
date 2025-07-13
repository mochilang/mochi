import 'package:yaml/yaml.dart';
import 'dart:io';
import 'dart:convert';

List<Map<String, dynamic>> people = [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}];

void main() {
  _save(people, "-", {"format": "json"});
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
