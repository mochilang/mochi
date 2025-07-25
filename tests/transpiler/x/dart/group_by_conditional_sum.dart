// Generated by Mochi transpiler
class S1 {
  final dynamic cat;
  final num share;
  const S1({required this.cat, required this.share});
}

class Item {
  final String cat;
  final int val;
  final bool flag;
  const Item({required this.cat, required this.val, required this.flag});
}

void main() {
  final List<Item> items = [Item(cat: "a", val: 10, flag: true), Item(cat: "a", val: 5, flag: false), Item(cat: "b", val: 20, flag: true)];
  final List<S1> result = (() {
  var groups = <String, Map<String, dynamic>>{};
  for (var i in items) {
    var key = i.cat;
    var ks = key.toString();
    var g = groups[ks];
    if (g == null) {
      g = {'key': key, 'items': []};
      groups[ks] = g;
    }
    (g['items'] as List).add(i);
  }
  var _list = groups.values.toList();
  _list.sort(((a, b) => a.key.compareTo(b.key)));
  var res = <S1>[];
  for (var g in _list) {
    res.add(S1(cat: g["key"], share: [for (var x in g["items"]) x.flag ? x.val : 0].isEmpty ? 0 : ([for (var x in g["items"]) x.flag ? x.val : 0].reduce((a, b) => a + b)) / [for (var x in g["items"]) x.val].isEmpty ? 0 : ([for (var x in g["items"]) x.val].reduce((a, b) => a + b))));
  }
  return res;
})();;
  print("[" + result.join(', ') + "]");
}
