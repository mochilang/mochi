// Generated by Mochi transpiler
import 'dart:convert';
import 'dart:io';

int _nowSeed = 0;
bool _nowSeeded = false;
void _initNow() {
  var s = Platform.environment['MOCHI_NOW_SEED'];
  if (s != null && s.isNotEmpty) {
    var v = int.tryParse(s);
    if (v != null) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
int _now() {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  return DateTime.now().microsecondsSinceEpoch;
}

class Client {
  String Base;
  String Host;
  int Port;
  String GroupFilter;
  Client({required this.Base, required this.Host, required this.Port, required this.GroupFilter});
}

void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  List<String> search_user(Map<String, List<String>> directory, String username) {
  return directory[username]!;
}
  void main() {
  final Client client = Client(Base: "dc=example,dc=com", Host: "ldap.example.com", Port: 389, GroupFilter: "(memberUid=%s)");
  final Map<String, List<String>> directory = {"username": ["admins", "users"], "john": ["users"]};
  final List<String> groups = search_user(directory, "username");
  if (groups.length > 0) {
    String out = "Groups: [";
    int i = 0;
    while (i < groups.length) {
    out = out + "\"" + groups[i] + "\"";
    if (i < groups.length - 1) {
    out = out + ", ";
  }
    i = i + 1;
  };
    out = out + "]";
    print(out);
  } else {
    print("User not found");
  }
}
  main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
