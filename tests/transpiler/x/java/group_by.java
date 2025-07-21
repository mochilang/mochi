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
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("age")) return true;
            if (k.equals("city")) return true;
            return false;
        }
    }

    static java.util.List<Result4> stats = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result4> _tmp = new java.util.ArrayList<>(); for (var person : people) { var _k = ((Integer) (person.get("city"))); String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(person); } java.util.ArrayList<Group2> list = new java.util.ArrayList<>(_groups.values()); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group2)list.get(i); _tmp.add(new Result4(g.key, g.items.size(), (((new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var p : g.items) { _tmp.add(((Integer) (p.get("age")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) % 1 == 0) ? (Object)(int)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var p : g.items) { _tmp.add(((Integer) (p.get("age")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) : (Object)(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var p : g.items) { _tmp.add(((Integer) (p.get("age")))); } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0))))); } addAll(_tmp);}};
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
        Object city;
        int count;
        Object avg_age;
        Result4(Object city, int count, Object avg_age) {
            this.city = city;
            this.count = count;
            this.avg_age = avg_age;
        }
        boolean containsKey(String k) {
            if (k.equals("city")) return true;
            if (k.equals("count")) return true;
            if (k.equals("avg_age")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- People grouped by city ---");
        for (var s : stats) {
            System.out.println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age);
        }
    }
}
