void main() {
  print(_count([1, 2, 3]));
}

int _count(dynamic v) {
    if (v is String) return v.runes.length;
    if (v is List) return v.length;
    if (v is Map) return v.length;
    try { var items = (v as dynamic).Items; if (items is List) return items.length; } catch (_) {}
    try { var items = (v as dynamic).items; if (items is List) return items.length; } catch (_) {}
    return 0;
}
