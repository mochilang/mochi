public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", 30, "Paris"), new Data1("Bob", 15, "Hanoi"), new Data1("Charlie", 65, "Paris"), new Data1("Diana", 45, "Hanoi"), new Data1("Eve", 70, "Paris"), new Data1("Frank", 22, "Hanoi")};
    static class Data1 {
        String name;
        int age;
        String city;
        Data1(String name, int age, String city) {
            this.name = name;
            this.age = age;
            this.city = city;
        }
    }

    static java.util.List<Result4> stats = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); for (var person : people) { var _k = person.city; String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(person); } for (var g : _groups.values()) { add(new Result4(g.key, g.items.size(), ((new java.util.ArrayList<Integer>() {{ for (var p : g.items) { add(p.age); }}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) % 1 == 0 ? (int)(new java.util.ArrayList<Integer>() {{ for (var p : g.items) { add(p.age); }}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) : (new java.util.ArrayList<Integer>() {{ for (var p : g.items) { add(p.age); }}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0))))); }}};
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
        int count;
        Object avg_age;
        Result4(Object city, int count, Object avg_age) {
            this.city = city;
            this.count = count;
            this.avg_age = avg_age;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- People grouped by city ---");
        for (var s : stats) {
            System.out.println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age);
        }
    }
}
