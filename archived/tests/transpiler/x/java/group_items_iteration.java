public class Main {
    static Data1[] data = new Data1[]{new Data1("a", 1), new Data1("a", 2), new Data1("b", 3)};
    static class Data1 {
        String tag;
        int val;
        Data1(String tag, int val) {
            this.tag = tag;
            this.val = val;
        }
        boolean containsKey(String k) {
            if (k.equals("tag")) return true;
            if (k.equals("val")) return true;
            return false;
        }
    }

    static java.util.List<Data1> groups = new java.util.ArrayList<Data1>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Data1> _tmp = new java.util.ArrayList<>(); for (var d : data) { var _k = ((Integer) (d.get("tag"))); String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(d); } java.util.ArrayList<Group2> list = new java.util.ArrayList<>(_groups.values()); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group2)list.get(i); _tmp.add(g); } addAll(_tmp);}};
    static class Group2 {
        String key;
        java.util.List<Data1> items;
        Group2(String key, java.util.List<Data1> items) {
            this.key = key;
            this.items = items;
        }
        boolean containsKey(String k) {
            if (k.equals("key")) return true;
            if (k.equals("items")) return true;
            return false;
        }
    }

    static Object[] tmp = new int[]{};
    static class Data3 {
         tag;
         total;
        Data3( tag,  total) {
            this.tag = tag;
            this.total = total;
        }
        boolean containsKey(String k) {
            if (k.equals("tag")) return true;
            if (k.equals("total")) return true;
            return false;
        }
    }

    static java.util.List<Object> result = new java.util.ArrayList<Object>() {{ java.util.ArrayList<Object> _tmp = new java.util.ArrayList<>(); for (var r : tmp) { _tmp.add(r); } java.util.ArrayList<Object> list = _tmp; list.sort((a, b) -> {Comparable _va = (Comparable)(a.tag); Comparable _vb = (Comparable)(b.tag); return _va.compareTo(_vb);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Object)list.get(i)); } addAll(_tmp);}};

    public static void main(String[] args) {
        for (var g : groups) {
            int total = 0;
            for (var x : g.items) {
                total = total + x.val;
            }
            tmp = java.util.stream.Stream.concat(java.util.Arrays.stream(tmp), java.util.stream.Stream.of(new Data3(g.key, total))).toArray(Object[]::new);
        }
        System.out.println(((java.util.List<?>)result).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
