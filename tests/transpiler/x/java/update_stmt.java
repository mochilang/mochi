public class Main {
    static class Person {
        String name;
        int age;
        String status;
        Person(String name, int age, String status) {
            this.name = name;
            this.age = age;
            this.status = status;
        }
        @Override public String toString() {
            return String.format("{'name': %s, 'age': %s, 'status': %s}", name, age, status);
        }
    }

    static Person[] people = new Person[]{new Person("Alice", 17, "minor"), new Person("Bob", 25, "unknown"), new Person("Charlie", 18, "unknown"), new Person("Diana", 16, "minor")};

    public static void main(String[] args) {
        for (int i = 0; i < people.length; i++) {
            var item = people[i];
            if (item.age >= 18) {
                item.status = "adult";
                item.age = item.age + 1;
            }
            people[i] = item;
        }
        System.out.println("ok");
    }
}
