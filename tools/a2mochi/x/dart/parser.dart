import 'dart:convert';
import 'dart:io';
import 'package:analyzer/dart/analysis/utilities.dart';

void main() async {
  final src = await stdin.transform(utf8.decoder).join();
  final unit = parseString(content: src).unit;
  final funcs = <Map<String, dynamic>>[];
  final classes = <Map<String, dynamic>>[];
  for (var d in unit.declarations) {
    if (d is ClassDeclaration) {
      final fields = <Map<String, String>>[];
      for (var m in d.members) {
        if (m is FieldDeclaration) {
          final type = m.fields.type?.toSource() ?? 'dynamic';
          for (var v in m.fields.variables) {
            fields.add({'name': v.name.lexeme, 'type': type});
          }
        }
      }
      final start = unit.lineInfo.getLocation(d.offset).lineNumber;
      final end = unit.lineInfo.getLocation(d.end).lineNumber;
      final doc = d.documentationComment?.tokens
              .map((t) => t.toString().replaceFirst('///', '').trim())
              .join('\n') ?? '';
      classes.add({
        'name': d.name.lexeme,
        'fields': fields,
        'start': start,
        'end': end,
        'doc': doc,
      });
    } else if (d is FunctionDeclaration) {
      final params = <Map<String, String>>[];
      final pList = d.functionExpression.parameters;
      if (pList != null) {
        for (var p in pList.parameters) {
          params.add({
            'name': p.identifier?.name ?? '',
            'type': p is SimpleFormalParameter && p.type != null ? p.type.toSource() : '',
          });
        }
      }
      final body = d.functionExpression.body.toSource();
      final start = unit.lineInfo.getLocation(d.offset).lineNumber;
      final end = unit.lineInfo.getLocation(d.end).lineNumber;
      final ret = d.returnType?.toSource() ?? '';
      final doc = d.documentationComment?.tokens
              .map((t) => t.toString().replaceFirst('///', '').trim())
              .join('\n') ?? '';
      funcs.add({
        'name': d.name.lexeme,
        'params': params,
        'ret': ret,
        'body': body.split('\n'),
        'start': start,
        'end': end,
        'doc': doc,
      });
    }
  }
  stdout.write(JsonEncoder.withIndent('', '  ').convert({
    'functions': funcs,
    'classes': classes,
  }));
}
