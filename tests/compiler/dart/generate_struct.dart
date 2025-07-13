import 'dart:convert';

Map<String, Function> _structParsers = {};

class Info {
  String msg;
  Info({required this.msg});
  factory Info.fromJson(Map<String,dynamic> m) {
    return Info(msg: m['msg'] as String);
  }
}

Info info = _genStruct<Info>("{\"msg\": \"hello\"}", "", null);

void main() {
  _structParsers['Info'] = (m) => Info.fromJson(m);
  
  print(info.msg);
}

Map<String, Function> _structParsers = {};
T _genStruct<T>(String prompt, String model, Map<String,dynamic>? params) {
    var data = jsonDecode(prompt) as Map<String, dynamic>;
    var fn = _structParsers[T.toString()];
    if (fn == null) throw Exception('unknown struct type $T');
    return Function.apply(fn, [data]) as T;
}
