public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", 30), new Data1("Bob", 15), new Data1("Charlie", 65), new Data1("Diana", 45)};
    static class Data1 {
        String name;
        int age;
        Data1(String name, int age) {
            this.name = name;
            this.age = age;
        }
    }

    static java.util.List<Result3> adults = new java.util.ArrayList<Result3>() {{ for (var person : people) { if (person.age >= 18) { add(new Result3(person.name, person.age, person.age >= 60)); } }}};
    static class Data2 {
         name;
         age;
        boolean is_senior;
        Data2( name,  age, boolean is_senior) {
            this.name = name;
            this.age = age;
            this.is_senior = is_senior;
        }
    }

    static class Result3 {
         name;
         age;
        int is_senior;
        Result3( name,  age, int is_senior) {
            this.name = name;
            this.age = age;
            this.is_senior = is_senior;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Adults ---");
        for (var person : adults) {
            System.out.println(person.name + " " + "is" + " " + person.age + " " + person.is_senior ? " (senior)" : "");
        }
    }
}
