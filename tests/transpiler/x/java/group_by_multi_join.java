public class Main {
    static Data1[] nations = new Data1[]{new Data1(1, "A"), new Data1(2, "B")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
        }
    }

    static Data2[] suppliers = new Data2[]{new Data2(1, 1), new Data2(2, 2)};
    static class Data2 {
        int id;
        int nation;
        Data2(int id, int nation) {
            this.id = id;
            this.nation = nation;
        }
    }

    static Data3[] partsupp = new Data3[]{new Data3(100, 1, 10.0, 2), new Data3(100, 2, 20.0, 1), new Data3(200, 1, 5.0, 3)};
    static class Data3 {
        int part;
        int supplier;
        double cost;
        int qty;
        Data3(int part, int supplier, double cost, int qty) {
            this.part = part;
            this.supplier = supplier;
            this.cost = cost;
            this.qty = qty;
        }
    }

    static java.util.List<Result5> filtered = new java.util.ArrayList<Result5>() {{ java.util.ArrayList<Result5> _tmp = new java.util.ArrayList<>(); for (var ps : partsupp) { for (var s : suppliers) { if (s.id == ps.supplier) { for (var n : nations) { if (n.id == s.nation) { if (n.name == "A") { _tmp.add(new Result5(ps.part, ps.cost * ps.qty)); } } } } } } java.util.ArrayList<Result5> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Result5)list.get(i)); } addAll(_tmp);}};
    static class Result5 {
        int part;
        double value;
        Result5(int part, double value) {
            this.part = part;
            this.value = value;
        }
    }

    static java.util.List<Result8> grouped = new java.util.ArrayList<Result8>() {{ java.util.LinkedHashMap<String,Group6> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result8> _tmp = new java.util.ArrayList<>(); for (var x : filtered) { var _k = x.part; String _ks = String.valueOf(_k); Group6 g = _groups.get(_ks); if (g == null) { g = new Group6(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(x); } java.util.ArrayList<Group6> list = new java.util.ArrayList<>(_groups.values()); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group6)list.get(i); _tmp.add(new Result8(g.key, (((new java.util.ArrayList<java.util.List<Result5>>() {{ java.util.ArrayList<java.util.List<Result5>> _tmp = new java.util.ArrayList<>(); for (var r : g.items) { _tmp.add(r.value); } java.util.ArrayList<java.util.List<Result5>> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((java.util.List<Result5>)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<java.util.List<Result5>>() {{ java.util.ArrayList<java.util.List<Result5>> _tmp = new java.util.ArrayList<>(); for (var r : g.items) { _tmp.add(r.value); } java.util.ArrayList<java.util.List<Result5>> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((java.util.List<Result5>)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<java.util.List<Result5>>() {{ java.util.ArrayList<java.util.List<Result5>> _tmp = new java.util.ArrayList<>(); for (var r : g.items) { _tmp.add(r.value); } java.util.ArrayList<java.util.List<Result5>> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((java.util.List<Result5>)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum())))); } addAll(_tmp);}};
    static class Group6 {
        Object key;
        java.util.List<java.util.List<Result5>> items;
        Group6(Object key, java.util.List<java.util.List<Result5>> items) {
            this.key = key;
            this.items = items;
        }
    }

    static class Result8 {
        Object part;
        Object total;
        Result8(Object part, Object total) {
            this.part = part;
            this.total = total;
        }
    }


    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)grouped).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
