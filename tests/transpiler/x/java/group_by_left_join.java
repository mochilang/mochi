public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob"), new Data1(3, "Charlie")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
        }
        boolean containsKey(String k) {
            if (k.equals("id")) return true;
            if (k.equals("name")) return true;
            return false;
        }
    }

    static Data2[] orders = new Data2[]{new Data2(100, 1), new Data2(101, 1), new Data2(102, 2)};
    static class Data2 {
        int id;
        int customerId;
        Data2(int id, int customerId) {
            this.id = id;
            this.customerId = customerId;
        }
        boolean containsKey(String k) {
            if (k.equals("id")) return true;
            if (k.equals("customerId")) return true;
            return false;
        }
    }

    static java.util.List<Result5> stats = new java.util.ArrayList<Result5>() {{ java.util.LinkedHashMap<String,Group3> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result5> _tmp = new java.util.ArrayList<>(); for (var c : customers) { for (var o : orders) { if (o.customerId == ((Integer) (c.get("id")))) { var _k = ((Integer) (c.get("name"))); String _ks = String.valueOf(_k); Group3 g = _groups.get(_ks); if (g == null) { g = new Group3(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item3(c, o)); } } } java.util.ArrayList<Group3> list = new java.util.ArrayList<>(_groups.values()); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group3)list.get(i); _tmp.add(new Result5(g.key, new java.util.ArrayList<Item3>() {{ java.util.ArrayList<Item3> _tmp = new java.util.ArrayList<>(); for (var r : g.items) { if (r.o) { _tmp.add(r); } } java.util.ArrayList<Item3> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Item3)list.get(i)); } addAll(_tmp);}}.length)); } addAll(_tmp);}};
    static class Item3 {
        Data1 c;
         o;
        Item3(Data1 c,  o) {
            this.c = c;
            this.o = o;
        }
        boolean containsKey(String k) {
            if (k.equals("c")) return true;
            if (k.equals("o")) return true;
            return false;
        }
    }

    static class Group3 {
        String key;
        java.util.List<Item3> items;
        Group3(String key, java.util.List<Item3> items) {
            this.key = key;
            this.items = items;
        }
        boolean containsKey(String k) {
            if (k.equals("key")) return true;
            if (k.equals("items")) return true;
            return false;
        }
    }

    static class Result5 {
        Object name;
        int count;
        Result5(Object name, int count) {
            this.name = name;
            this.count = count;
        }
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("count")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Group Left Join ---");
        for (var s : stats) {
            System.out.println(s.name + " " + "orders:" + " " + s.count);
        }
    }
}
