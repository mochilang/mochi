public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", "Paris"), new Data1("Bob", "Hanoi"), new Data1("Charlie", "Paris"), new Data1("Diana", "Hanoi"), new Data1("Eve", "Paris"), new Data1("Frank", "Hanoi"), new Data1("George", "Paris")};
    static class Data1 {
        String name;
        String city;
        Data1(String name, String city) {
            this.name = name;
            this.city = city;
        }
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("city")) return true;
            return false;
        }
    }

    static java.util.List<Result4> big = new java.util.ArrayList<Result4>() {{ java.util.LinkedHashMap<String,Group2> _groups = new java.util.LinkedHashMap<>(); java.util.ArrayList<Result4> _tmp = new java.util.ArrayList<>(); for (var p : people) { var _k = ((Integer) (p.get("city"))); String _ks = String.valueOf(_k); Group2 g = _groups.get(_ks); if (g == null) { g = new Group2(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); } g.items.add(p); } java.util.ArrayList<Group2> list = new java.util.ArrayList<>(_groups.values()); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; var g = (Group2)list.get(i); if (g.items.size() >= 4) { _tmp.add(new Result4(g.key, g.items.size())); } } addAll(_tmp);}};
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
        int num;
        Result4(Object city, int num) {
            this.city = city;
            this.num = num;
        }
        boolean containsKey(String k) {
            if (k.equals("city")) return true;
            if (k.equals("num")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        json(big);
    }
}
