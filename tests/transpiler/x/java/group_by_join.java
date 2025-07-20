public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob")};
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

    static java.util.List<Result4> stats = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group5> _groups = new java.util.LinkedHashMap<>(); for (var o : orders) { for (var c : customers) { if (o.customerId == c.id) { var _k = c.name; String _ks = String.valueOf(_k); Group5 g = _groups.get(_ks); if (g == null) { g = new Group5(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item5(o, c)); } } } for (var g : _groups.values()) { Group5 g = g; add(new Result4(g.key, count(g))); }}};
    static class Data3 {
         name;
        int count;
        Data3( name, int count) {
            this.name = name;
            this.count = count;
        }
    }

    static class Result4 {
         name;
        int count;
        Result4( name, int count) {
            this.name = name;
            this.count = count;
        }
    }

    static class Item5 {
        java.util.Map o;
         c;
        Item5(java.util.Map o,  c) {
            this.o = o;
            this.c = c;
        }
    }

    static class Group5 {
         key;
        java.util.List<Item5> items;
        Group5( key, java.util.List<Item5> items) {
            this.key = key;
            this.items = items;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Orders per customer ---");
        for (var s : stats) {
            System.out.println(s.name + " " + "orders:" + " " + s.count);
        }
    }
}
