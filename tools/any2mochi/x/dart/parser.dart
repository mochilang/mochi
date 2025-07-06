import 'dart:convert';
import 'dart:io';
import 'package:analyzer/dart/analysis/utilities.dart';

void main() async {
  final src = await stdin.transform(utf8.decoder).join();
  final unit = parseString(content: src).unit;
  final funcs = <Map<String, dynamic>>[];
  for (var d in unit.declarations) {
    if (d is FunctionDeclaration) {
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
  final ast = {'functions': funcs};
  stdout.write(JsonEncoder.withIndent('', '  ').convert(ast));
}
