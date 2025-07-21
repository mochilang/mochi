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

    static java.util.List<Result4> big = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); for (var p : people) { var _k = p.city; String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(p); } for (var g : _groups.values()) { if (g.items.size() >= 4) { add(new Result4(g.key, g.items.size())); } }}};
    static class Group2 {
        String key;
        java.util.List<Data1> items;
        Group2(String key, java.util.List<Data1> items) {
            this.key = key;
            this.items = items;
        }
    }

    static class Result4 {
        Object city;
        int num;
        Result4(Object city, int num) {
            this.city = city;
            this.num = num;
        }
    }


    public static void main(String[] args) {
        json(big);
    }
}
