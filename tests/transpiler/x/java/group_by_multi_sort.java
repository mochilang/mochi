public class Main {
    static Data1[] items = new Data1[]{new Data1("x", 1, 2), new Data1("x", 2, 3), new Data1("y", 1, 4), new Data1("y", 2, 1)};
    static class Data1 {
        String a;
        int b;
        int val;
        Data1(String a, int b, int val) {
            this.a = a;
            this.b = b;
            this.val = val;
        }
        @Override public String toString() {
            return String.format("{'a': '%s', 'b': %s, 'val': %s}", q(a), q(b), q(val));
        }
    }

    static java.util.List<Result5> grouped = new java.util.ArrayList<Result5>() {{ java.util.LinkedHashMap<String,Group3> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result5> _tmp = new java.util.ArrayList<>(); for (var i : items) { var _k = new Data2(i.a, i.b); String _ks = String.valueOf(_k); Group3 g = _groups.get(_ks); if (g == null) { g = new Group3(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(i); } java.util.ArrayList<Group3> list = new java.util.ArrayList<>(_groups.values()); java.util.ArrayList<Result5> _res = new java.util.ArrayList<>(); list.sort((a, b) -> {Comparable _va = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); Comparable _vb = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); return _vb.compareTo(_va);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group3)list.get(i); _res.add(new Result5(g.key.a, g.key.b, (((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.val); } java.util.ArrayList<Integer> list = _tmp; java.util.ArrayList<Integer> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Integer)list.get(i)); } addAll(_res);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum())))); } addAll(_res);}};
    static class Data2 {
        String a;
        int b;
        Data2(String a, int b) {
            this.a = a;
            this.b = b;
        }
        @Override public String toString() {
            return String.format("{'a': '%s', 'b': %s}", q(a), q(b));
        }
    }

    static class Group3 {
        Data2 key;
        java.util.List<Data1> items;
        Group3(Data2 key, java.util.List<Data1> items) {
            this.key = key;
            this.items = items;
        }
        @Override public String toString() {
            return String.format("{'key': %s, 'items': %s}", q(key), q(items));
        }
    }

    static class Result5 {
        Object a;
        Object b;
        Object total;
        Result5(Object a, Object b, Object total) {
            this.a = a;
            this.b = b;
            this.total = total;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'b': %s, 'total': %s}", q(a), q(b), q(total));
        }
    }


    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        System.out.println(("[" + ((java.util.List<?>)grouped).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(", ")) + "]"));
    }
}
