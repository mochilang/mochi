import 'dart:convert';
import 'dart:io';
import 'package:analyzer/dart/analysis/utilities.dart';

void main() async {
  final src = await stdin.transform(utf8.decoder).join();
  final unit = parseString(content: src).unit;
  final funcs = <Map<String, dynamic>>[];
  final classes = <Map<String, dynamic>>[];
  final enums = <Map<String, dynamic>>[];
  for (var d in unit.declarations) {
    if (d is ClassDeclaration) {
      final name = d.name.lexeme;
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
        'name': name,
        'fields': fields,
        'start': start,
        'end': end,
        'doc': doc,
      });
    } else if (d is EnumDeclaration) {
      final name = d.name.lexeme;
      final members = <String>[];
      for (var c in d.constants) {
        members.add(c.name.lexeme);
      }
      final start = unit.lineInfo.getLocation(d.offset).lineNumber;
      final end = unit.lineInfo.getLocation(d.end).lineNumber;
      final doc = d.documentationComment?.tokens
              .map((t) => t.toString().replaceFirst('///', '').trim())
              .join('\n') ?? '';
      enums.add({
        'name': name,
        'members': members,
        'start': start,
        'end': end,
        'doc': doc,
      });
    } else if (d is FunctionDeclaration) {
      final name = d.name.lexeme;
      final params = <Map<String, String>>[];
      if (d.functionExpression.parameters != null) {
        for (var p in d.functionExpression.parameters!.parameters) {
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
      final doc = d.documentationComment?.tokens.map((t) => t.toString().replaceFirst('///', '').trim()).join('\n') ?? '';
      funcs.add({
        'name': name,
        'params': params,
        'ret': ret,
        'body': body.split('\n'),
        'start': start,
        'end': end,
        'doc': doc,
      });
    }
  }
  final ast = {
    'functions': funcs,
    'classes': classes,
    'enums': enums,
  };
  stdout.write(JsonEncoder.withIndent('', '  ').convert(ast));
}
