public class Main {
    static class Person {
        String name;
        int age;
        String email;
        Person(String name, int age, String email) {
            this.name = name;
            this.age = age;
            this.email = email;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'age': %s, 'email': '%s'}", q(name), q(age), q(email));
        }
    }

    static Person[] people = new Person[]{new Person("Alice", 30, "alice@example.com"), new Person("Bob", 15, "bob@example.com"), new Person("Charlie", 20, "charlie@example.com")};
    static java.util.List<Result2> adults = new java.util.ArrayList<Result2>() {{ java.util.ArrayList<Result2> _tmp = new java.util.ArrayList<>(); for (var p : people) { if (p.age >= 18) { _tmp.add(new Result2(p.name, p.email)); } } java.util.ArrayList<Result2> list = _tmp; java.util.ArrayList<Result2> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Result2)list.get(i)); } addAll(_res);}};
    static class Result2 {
        String name;
        String email;
        Result2(String name, String email) {
            this.name = name;
            this.email = email;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'email': '%s'}", q(name), q(email));
        }
    }


    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        for (var a : adults) {
            System.out.println(a.name + " " + a.email);
        }
    }
}
