public class Main {
    static Data1[] nation = new Data1[]{new Data1(1, "BRAZIL")};
    static class Data1 {
        int n_nationkey;
        String n_name;
        Data1(int n_nationkey, String n_name) {
            this.n_nationkey = n_nationkey;
            this.n_name = n_name;
        }
    }

    static Data2[] customer = new Data2[]{new Data2(1, "Alice", 100.0, 1, "123 St", "123-456", "Loyal")};
    static class Data2 {
        int c_custkey;
        String c_name;
        double c_acctbal;
        int c_nationkey;
        String c_address;
        String c_phone;
        String c_comment;
        Data2(int c_custkey, String c_name, double c_acctbal, int c_nationkey, String c_address, String c_phone, String c_comment) {
            this.c_custkey = c_custkey;
            this.c_name = c_name;
            this.c_acctbal = c_acctbal;
            this.c_nationkey = c_nationkey;
            this.c_address = c_address;
            this.c_phone = c_phone;
            this.c_comment = c_comment;
        }
    }

    static Data3[] orders = new Data3[]{new Data3(1000, 1, "1993-10-15"), new Data3(2000, 1, "1994-01-02")};
    static class Data3 {
        int o_orderkey;
        int o_custkey;
        String o_orderdate;
        Data3(int o_orderkey, int o_custkey, String o_orderdate) {
            this.o_orderkey = o_orderkey;
            this.o_custkey = o_custkey;
            this.o_orderdate = o_orderdate;
        }
    }

    static Data4[] lineitem = new Data4[]{new Data4(1000, "R", 1000.0, 0.1), new Data4(2000, "N", 500.0, 0.0)};
    static class Data4 {
        int l_orderkey;
        String l_returnflag;
        double l_extendedprice;
        double l_discount;
        Data4(int l_orderkey, String l_returnflag, double l_extendedprice, double l_discount) {
            this.l_orderkey = l_orderkey;
            this.l_returnflag = l_returnflag;
            this.l_extendedprice = l_extendedprice;
            this.l_discount = l_discount;
        }
    }

    static String start_date = "1993-10-01";
    static String end_date = "1994-01-01";
    static java.util.List<Result8> result = new java.util.ArrayList<Result8>() {{ java.util.LinkedHashMap<String,Group6> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result8> _tmp = new java.util.ArrayList<>(); for (var c : customer) { for (var o : orders) { if (o.o_custkey == c.c_custkey) { for (var l : lineitem) { if (l.l_orderkey == o.o_orderkey) { for (var n : nation) { if (n.n_nationkey == c.c_nationkey) { if ((o.o_orderdate.compareTo(start_date) >= 0) && o.o_orderdate < end_date && l.l_returnflag == "R") { var _k = new Data5(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, n.n_name); String _ks = String.valueOf(_k); Group6 g = _groups.get(_ks); if (g == null) { g = new Group6(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item6(c, o, l, n)); } } } } } } } } java.util.ArrayList<Group6> list = new java.util.ArrayList<>(_groups.values()); list.sort((a, b) -> {Comparable _va = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : a.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); Comparable _vb = (Comparable)((((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : b.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); return _vb.compareTo(_va);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group6)list.get(i); _tmp.add(new Result8(g.key.c_custkey, g.key.c_name, (((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : g.items) { _tmp.add(x.l.l_extendedprice * (1 - x.l.l_discount)); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum())), g.key.c_acctbal, g.key.n_name, g.key.c_address, g.key.c_phone, g.key.c_comment)); } addAll(_tmp);}};
    static class Data5 {
        int c_custkey;
        String c_name;
        double c_acctbal;
        String c_address;
        String c_phone;
        String c_comment;
        String n_name;
        Data5(int c_custkey, String c_name, double c_acctbal, String c_address, String c_phone, String c_comment, String n_name) {
            this.c_custkey = c_custkey;
            this.c_name = c_name;
            this.c_acctbal = c_acctbal;
            this.c_address = c_address;
            this.c_phone = c_phone;
            this.c_comment = c_comment;
            this.n_name = n_name;
        }
    }

    static class Item6 {
        Data2 c;
        Data3 o;
        Data4 l;
        Data1 n;
        Item6(Data2 c, Data3 o, Data4 l, Data1 n) {
            this.c = c;
            this.o = o;
            this.l = l;
            this.n = n;
        }
    }

    static class Group6 {
        Data5 key;
        java.util.List<Item6> items;
        Group6(Data5 key, java.util.List<Item6> items) {
            this.key = key;
            this.items = items;
        }
    }

    static class Result8 {
        Object c_custkey;
        Object c_name;
        Object revenue;
        Object c_acctbal;
        Object n_name;
        Object c_address;
        Object c_phone;
        Object c_comment;
        Result8(Object c_custkey, Object c_name, Object revenue, Object c_acctbal, Object n_name, Object c_address, Object c_phone, Object c_comment) {
            this.c_custkey = c_custkey;
            this.c_name = c_name;
            this.revenue = revenue;
            this.c_acctbal = c_acctbal;
            this.n_name = n_name;
            this.c_address = c_address;
            this.c_phone = c_phone;
            this.c_comment = c_comment;
        }
    }


    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)result).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
