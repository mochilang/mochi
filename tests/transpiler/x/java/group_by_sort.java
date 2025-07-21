public class Main {
    static Data1[] items = new Data1[]{new Data1("a", 3), new Data1("a", 1), new Data1("b", 5), new Data1("b", 2)};
    static class Data1 {
        String cat;
        int val;
        Data1(String cat, int val) {
            this.cat = cat;
            this.val = val;
        }
        boolean containsKey(String k) {
            if (k.equals("cat")) return true;
            if (k.equals("val")) return true;
            return false;
        }
    }

    static java.util.List<Result4> grouped = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result4> _tmp = new java.util.ArrayList<>(); for (var i : items) { var _k = ((Integer) (i.get("cat"))); String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(i); } java.util.ArrayList<Group2> list = new java.util.ArrayList<>(_groups.values()); list.sort((a, b) -> {Comparable _va = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); Comparable _vb = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); return _vb.compareTo(_va);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group2)list.get(i); _tmp.add(new Result4(g.key, (((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(((Integer) (x.get("val")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum())))); } addAll(_tmp);}};
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

    static class Result4 {
        Object cat;
        Object total;
        Result4(Object cat, Object total) {
            this.cat = cat;
            this.total = total;
        }
        boolean containsKey(String k) {
            if (k.equals("cat")) return true;
            if (k.equals("total")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)grouped).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
