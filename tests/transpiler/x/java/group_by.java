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

    static java.util.List<Result3> stats = new java.util.ArrayList<Result3>() {{ java.util.LinkedHashMap<String,Group4> _groups = new java.util.LinkedHashMap<>(); for (var person : people) { var _k = person.city; String _ks = String.valueOf(_k); Group4 g = _groups.get(_ks); if (g == null) { g = new Group4(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(new Item4(person)); } for (var g : _groups.values()) { Group4 g = g; add(new Result3(g.key, count(g), avg(new java.util.ArrayList<java.util.Map>() {{ for (var p : g) { add(p.age); }}}))); }}};
    static class Data2 {
         city;
        int count;
         avg_age;
        Data2( city, int count,  avg_age) {
            this.city = city;
            this.count = count;
            this.avg_age = avg_age;
        }
    }

    static class Result3 {
         city;
        int count;
         avg_age;
        Result3( city, int count,  avg_age) {
            this.city = city;
            this.count = count;
            this.avg_age = avg_age;
        }
    }

    static class Item4 {
        java.util.Map person;
        Item4(java.util.Map person) {
            this.person = person;
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
        System.out.println("--- People grouped by city ---");
        for (var s : stats) {
            System.out.println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age);
        }
    }
}
