import 'dart:io';
import 'dart:convert';

Map<String, Function> _structParsers = {};

class Msg {
  String message;
  Msg({required this.message});
  factory Msg.fromJson(Map<String,dynamic> m) {
    return Msg(message: m['message'] as String);
  }
}

Msg data = await _fetch("file://tests/compiler/dart/fetch_options.json", {"method": "GET", "headers": {"User-Agent": "Mochi"}, "query": {"q": "foo"}, "body": {"foo": true}, "timeout": 5});

Future<void> main() async {
  _structParsers['Msg'] = (m) => Msg.fromJson(m);
  
  print(data.message);
  await _waitAll();
}

Future<dynamic> _fetch(String url, Map<String,dynamic>? opts) async {
    var method = opts?['method']?.toString() ?? 'GET';
    Uri uri = Uri.parse(url);
    if (opts?['query'] != null) {
        var q = (opts!['query'] as Map).map((k,v)=>MapEntry(k.toString(), v.toString()));
        uri = uri.replace(queryParameters: {...uri.queryParameters, ...q});
    }
    var client = HttpClient();
    if (opts?['timeout'] != null) {
        var t = Duration(seconds: (opts!['timeout'] is num ? opts['timeout'].round() : int.parse(opts['timeout'].toString())));
        client.connectionTimeout = t;
    }
    var req = await client.openUrl(method, uri);
    if (opts?['headers'] != null) {
        for (var e in (opts!['headers'] as Map).entries) {
            req.headers.set(e.key.toString(), e.value.toString());
        }
    }
    if (opts != null && opts.containsKey('body')) {
        req.headers.contentType = ContentType('application', 'json', charset: 'utf-8');
        req.write(jsonEncode(opts['body']));
    }
    var resp = await req.close();
    var text = await resp.transform(utf8.decoder).join();
    client.close();
    return jsonDecode(text);
}

List<Future<dynamic>> _pending = [];
Future<void> _waitAll() async {
    await Future.wait(_pending);
}
