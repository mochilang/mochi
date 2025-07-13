List<int> a = [1, 2, 3];

List<int> b = [3, 4];

void main() {
  print(_union(a, b));
  print(_except(a, b));
  print(_intersect(a, b));
  print(_union([1, 2], [2, 3]).length);
}

List<dynamic> _except(List<dynamic> a, List<dynamic> b) {
    var res = <dynamic>[];
    for (var it in a) {
        if (!b.contains(it)) {
            res.add(it);
        }
    }
    return res;
}

List<dynamic> _intersect(List<dynamic> a, List<dynamic> b) {
    var res = <dynamic>[];
    for (var it in a) {
        if (b.contains(it) && !res.contains(it)) {
            res.add(it);
        }
    }
    return res;
}

List<dynamic> _union(List<dynamic> a, List<dynamic> b) {
    var res = [...a];
    for (var it in b) {
        if (!res.contains(it)) {
            res.add(it);
        }
    }
    return res;
}
