public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", "Paris"), new Data1("Bob", "Hanoi"), new Data1("Charlie", "Paris"), new Data1("Diana", "Hanoi"), new Data1("Eve", "Paris"), new Data1("Frank", "Hanoi"), new Data1("George", "Paris")};
    static class Data1 {
        String name;
        String city;
        Data1(String name, String city) {
            this.name = name;
            this.city = city;
        }
    }

    static java.util.List<Result3> big = new java.util.ArrayList<Result3>() {{ java.util.LinkedHashMap<String,Group4> _groups = new java.util.LinkedHashMap<>(); for (var p : people) { var _k = p.city; String _ks = String.valueOf(_k); Group4 g = _groups.get(_ks); if (g == null) { g = new Group4(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item4(p)); } for (var g : _groups.values()) { Group4 g = g; if (count(g) >= 4) { add(new Result3(g.key, count(g))); } }}};
    static class Data2 {
         city;
        int num;
        Data2( city, int num) {
            this.city = city;
            this.num = num;
        }
    }

    static class Result3 {
         city;
        int num;
        Result3( city, int num) {
            this.city = city;
            this.num = num;
        }
    }

    static class Item4 {
        java.util.Map p;
        Item4(java.util.Map p) {
            this.p = p;
        }
    }

    static class Group4 {
         key;
        java.util.List<Item4> items;
        Group4( key, java.util.List<Item4> items) {
            this.key = key;
            this.items = items;
        }
    }


    public static void main(String[] args) {
        json(big);
    }
}
