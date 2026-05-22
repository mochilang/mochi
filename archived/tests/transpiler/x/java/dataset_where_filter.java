public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", 30), new Data1("Bob", 15), new Data1("Charlie", 65), new Data1("Diana", 45)};
    static class Data1 {
        String name;
        int age;
        Data1(String name, int age) {
            this.name = name;
            this.age = age;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'age': %s}", q(name), q(age));
        }
    }

    static java.util.List<Result3> adults = new java.util.ArrayList<Result3>() {{ java.util.ArrayList<Result3> _tmp = new java.util.ArrayList<>(); for (var person : people) { if (person.age >= 18) { _tmp.add(new Result3(person.name, person.age, person.age >= 60)); } } java.util.ArrayList<Result3> list = _tmp; java.util.ArrayList<Result3> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Result3)list.get(i)); } addAll(_res);}};
    static class Result3 {
        String name;
        int age;
        boolean is_senior;
        Result3(String name, int age, boolean is_senior) {
            this.name = name;
            this.age = age;
            this.is_senior = is_senior;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'age': %s, 'is_senior': %s}", q(name), q(age), q(is_senior));
        }
    }


    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        System.out.println("--- Adults ---");
        for (var person : adults) {
            System.out.println(person.name + " " + "is" + " " + person.age + " " + (person.is_senior ? " (senior)" : ""));
        }
    }
}
