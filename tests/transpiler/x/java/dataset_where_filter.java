public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", 30), new Data1("Bob", 15), new Data1("Charlie", 65), new Data1("Diana", 45)};
    static class Data1 {
        String name;
        int age;
        Data1(String name, int age) {
            this.name = name;
            this.age = age;
        }
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("age")) return true;
            return false;
        }
    }

    static java.util.List<Result3> adults = new java.util.ArrayList<Result3>() {{ java.util.ArrayList<Result3> _tmp = new java.util.ArrayList<>(); for (var person : people) { if (((Integer) (person.get("age"))) >= 18) { _tmp.add(new Result3(((Integer) (person.get("name"))), ((Integer) (person.get("age"))), ((Integer) (person.get("age"))) >= 60)); } } java.util.ArrayList<Result3> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Result3)list.get(i)); } addAll(_tmp);}};
    static class Result3 {
        String name;
        int age;
        int is_senior;
        Result3(String name, int age, int is_senior) {
            this.name = name;
            this.age = age;
            this.is_senior = is_senior;
        }
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("age")) return true;
            if (k.equals("is_senior")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Adults ---");
        for (var person : adults) {
            System.out.println(((Integer) (person.get("name"))) + " " + "is" + " " + ((Integer) (person.get("age"))) + " " + ((Integer) (person.get("is_senior"))) ? " (senior)" : "");
        }
    }
}
