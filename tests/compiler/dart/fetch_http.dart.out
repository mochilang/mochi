import 'dart:convert';
import 'dart:io';

Map<String, Function> _structParsers = {};

class Todo {
  int userId;
  int id;
  String title;
  bool completed;
  Todo({required this.userId, required this.id, required this.title, required this.completed});
  factory Todo.fromJson(Map<String,dynamic> m) {
    return Todo(userId: m['userId'] as int, id: m['id'] as int, title: m['title'] as String, completed: m['completed'] as bool);
  }
}

Todo todo = Todo.fromJson((await _fetch("https://jsonplaceholder.typicode.com/todos/1", null)) as Map<String,dynamic>);

Future<void> main() async {
  _structParsers['Todo'] = (m) => Todo.fromJson(m);
  
  print(todo.title);
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
