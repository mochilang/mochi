public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob"), new Data1(3, "Charlie")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
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
    }

    static java.util.List<Result5> stats = new java.util.ArrayList<Result5>() {{ java.util.LinkedHashMap<String,Group3> _groups = new java.util.LinkedHashMap<>(); for (var c : customers) { for (var o : orders) { if (o.customerId == c.id) { var _k = c.name; String _ks = String.valueOf(_k); Group3 g = _groups.get(_ks); if (g == null) { g = new Group3(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item3(c, o)); } } } for (var g : _groups.values()) { add(new Result5(g.key, new java.util.ArrayList<Item3>() {{ for (var r : g.items) { if (r.o) { add(r); } }}}.length)); }}};
    static class Item3 {
        Data1 c;
         o;
        Item3(Data1 c,  o) {
            this.c = c;
            this.o = o;
        }
    }

    static class Group3 {
        String key;
        java.util.List<Item3> items;
        Group3(String key, java.util.List<Item3> items) {
            this.key = key;
            this.items = items;
        }
    }

    static class Result5 {
        Object name;
        int count;
        Result5(Object name, int count) {
            this.name = name;
            this.count = count;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Group Left Join ---");
        for (var s : stats) {
            System.out.println(s.name + " " + "orders:" + " " + s.count);
        }
    }
}
