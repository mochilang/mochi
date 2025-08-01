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

String _substr(String s, int start, int end) {
  var n = s.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (start > n) start = n;
  if (end < 0) end = 0;
  if (end > n) end = n;
  if (start > end) start = end;
  return s.substring(start, end);
}

String _repeat(String s, int n) => List.filled(n, s).join();

num pow10(int n) {
  num r = 1.0;
  int i = 0;
  while (i < n) {
    r = r * 10.0;
    i = i + 1;
  }
  return r;
}

String formatFloat(num f, int prec) {
  num scale = pow10(prec);
  num scaled = f * scale + 0.5;
  int n = (scaled).toInt();
  String digits = (n).toString();
  while (digits.length <= prec) {
    digits = "0" + digits;
  }
  String intPart = _substr(digits, 0, digits.length - prec);
  String fracPart = _substr(digits, digits.length - prec, digits.length);
  return intPart + "." + fracPart;
}

String padLeft(String s, int w) {
  String res = "";
  int n = w - s.length;
  while (n > 0) {
    res = res + " ";
    n = n - 1;
  }
  return res + s;
}

String repeat(String ch, int n) {
  String s = "";
  int i = 0;
  while (i < n) {
    s = s + ch;
    i = i + 1;
  }
  return s;
}

num toFloat(int i) {
  return (i).toDouble();
}

Map<String, dynamic> newNode(String name, int weight, num coverage) {
  return {"name": name, "weight": weight, "coverage": coverage, "children": []};
}

void addChildren(Map<String, dynamic> n, List<Map<String, dynamic>> nodes) {
  List<dynamic> cs = List<dynamic>.from(n["children"]!);
  for (Map<String, dynamic> node in nodes) {
    cs = [...cs, node];
  }
  n["children"] = cs;
}

void setCoverage(Map<String, dynamic> n, num value) {
  n["coverage"] = value;
}

num computeCoverage(Map<String, dynamic> n) {
  List<dynamic> cs = List<dynamic>.from(n["children"]!);
  if (cs.length == 0) {
    return n["coverage"]! as num;
  }
  num v1 = 0.0;
  int v2 = 0;
  for (var node in cs) {
    Map<String, dynamic> m = node as Map<String, dynamic>;
    num c = computeCoverage(m);
    v1 = v1 + toFloat(m["weight"]! as int) * c;
    v2 = v2 + (m["weight"]! as int);
  }
  return v1 / toFloat(v2);
}

String spaces(int n) {
  return _repeat(" ", n);
}

void show(Map<String, dynamic> n, int level) {
  int indent = level * 4;
  String name = n["name"]! as String;
  int nl = name.length + indent;
  String line = spaces(indent) + name;
  line = line + spaces(32 - nl) + "|  ";
  line = line + padLeft((n["weight"]! as int).toString(), 3) + "   | ";
  line = line + formatFloat(computeCoverage(n), 6) + " |";
  print(line);
  List<dynamic> cs = List<dynamic>.from(n["children"]!);
  for (var child in cs) {
    show(child as Map<String, dynamic>, level + 1);
  }
}

Map<String, dynamic> house1 = newNode("house1", 40, 0.0);
Map<String, dynamic> house2 = newNode("house2", 60, 0.0);
Map<String, dynamic> h1_bedrooms = newNode("bedrooms", 1, 0.25);
Map<String, dynamic> h1_bathrooms = newNode("bathrooms", 1, 0.0);
Map<String, dynamic> h1_attic = newNode("attic", 1, 0.75);
Map<String, dynamic> h1_kitchen = newNode("kitchen", 1, 0.1);
Map<String, dynamic> h1_living_rooms = newNode("living_rooms", 1, 0.0);
Map<String, dynamic> h1_basement = newNode("basement", 1, 0.0);
Map<String, dynamic> h1_garage = newNode("garage", 1, 0.0);
Map<String, dynamic> h1_garden = newNode("garden", 1, 0.8);
Map<String, dynamic> h2_upstairs = newNode("upstairs", 1, 0.0);
Map<String, dynamic> h2_groundfloor = newNode("groundfloor", 1, 0.0);
Map<String, dynamic> h2_basement = newNode("basement", 1, 0.0);
Map<String, dynamic> h1_bathroom1 = newNode("bathroom1", 1, 0.5);
Map<String, dynamic> h1_bathroom2 = newNode("bathroom2", 1, 0.0);
Map<String, dynamic> h1_outside = newNode("outside_lavatory", 1, 1.0);
Map<String, dynamic> h1_lounge = newNode("lounge", 1, 0.0);
Map<String, dynamic> h1_dining = newNode("dining_room", 1, 0.0);
Map<String, dynamic> h1_conservatory = newNode("conservatory", 1, 0.0);
Map<String, dynamic> h1_playroom = newNode("playroom", 1, 1.0);
Map<String, dynamic> h2_bedrooms = newNode("bedrooms", 1, 0.0);
Map<String, dynamic> h2_bathroom = newNode("bathroom", 1, 0.0);
Map<String, dynamic> h2_toilet = newNode("toilet", 1, 0.0);
Map<String, dynamic> h2_attics = newNode("attics", 1, 0.6);
Map<String, dynamic> h2_kitchen = newNode("kitchen", 1, 0.0);
Map<String, dynamic> h2_living_rooms = newNode("living_rooms", 1, 0.0);
Map<String, dynamic> h2_wet_room = newNode("wet_room_&_toilet", 1, 0.0);
Map<String, dynamic> h2_garage = newNode("garage", 1, 0.0);
Map<String, dynamic> h2_garden = newNode("garden", 1, 0.9);
Map<String, dynamic> h2_hot_tub = newNode("hot_tub_suite", 1, 1.0);
Map<String, dynamic> h2_cellars = newNode("cellars", 1, 1.0);
Map<String, dynamic> h2_wine_cellar = newNode("wine_cellar", 1, 1.0);
Map<String, dynamic> h2_cinema = newNode("cinema", 1, 0.75);
Map<String, dynamic> h2_suite1 = newNode("suite_1", 1, 0.0);
Map<String, dynamic> h2_suite2 = newNode("suite_2", 1, 0.0);
Map<String, dynamic> h2_bedroom3 = newNode("bedroom_3", 1, 0.0);
Map<String, dynamic> h2_bedroom4 = newNode("bedroom_4", 1, 0.0);
Map<String, dynamic> h2_lounge = newNode("lounge", 1, 0.0);
Map<String, dynamic> h2_dining = newNode("dining_room", 1, 0.0);
Map<String, dynamic> h2_conservatory = newNode("conservatory", 1, 0.0);
Map<String, dynamic> h2_playroom = newNode("playroom", 1, 0.0);
void _main() {
  Map<String, dynamic> cleaning = newNode("cleaning", 1, 0.0);
  addChildren(h1_bathrooms, [h1_bathroom1, h1_bathroom2, h1_outside]);
  addChildren(h1_living_rooms, [h1_lounge, h1_dining, h1_conservatory, h1_playroom]);
  addChildren(house1, [h1_bedrooms, h1_bathrooms, h1_attic, h1_kitchen, h1_living_rooms, h1_basement, h1_garage, h1_garden]);
  addChildren(h2_bedrooms, [h2_suite1, h2_suite2, h2_bedroom3, h2_bedroom4]);
  addChildren(h2_upstairs, [h2_bedrooms, h2_bathroom, h2_toilet, h2_attics]);
  addChildren(h2_living_rooms, [h2_lounge, h2_dining, h2_conservatory, h2_playroom]);
  addChildren(h2_groundfloor, [h2_kitchen, h2_living_rooms, h2_wet_room, h2_garage, h2_garden, h2_hot_tub]);
  addChildren(h2_basement, [h2_cellars, h2_wine_cellar, h2_cinema]);
  addChildren(house2, [h2_upstairs, h2_groundfloor, h2_basement]);
  addChildren(cleaning, [house1, house2]);
  num topCoverage = computeCoverage(cleaning);
  print("TOP COVERAGE = " + formatFloat(topCoverage, 6));
  print("");
  print("NAME HIERARCHY                 | WEIGHT | COVERAGE |");
  show(cleaning, 0);
  setCoverage(h2_cinema, 1.0);
  num diff = computeCoverage(cleaning) - topCoverage;
  print("");
  print("If the coverage of the Cinema node were increased from 0.75 to 1");
  print("the top level coverage would increase by " + formatFloat(diff, 6) + " to " + formatFloat(topCoverage + diff, 6));
  setCoverage(h2_cinema, 0.75);
}

void _start() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "_start"}));
}

void main() => _start();
