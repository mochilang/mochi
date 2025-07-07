import java.util.*;

class Person {
    String name;
    int age;
    boolean isSenior;
    Person(String name, int age) { this.name = name; this.age = age; this.isSenior = age >= 60; }
}

public class DatasetWhereFilter {
    public static void main(String[] args) {
        List<Person> people = Arrays.asList(
            new Person("Alice", 30),
            new Person("Bob", 15),
            new Person("Charlie", 65),
            new Person("Diana", 45)
        );

        List<Person> adults = new ArrayList<>();
        for (Person p : people) {
            if (p.age >= 18) {
                adults.add(new Person(p.name, p.age));
            }
        }

        System.out.println("--- Adults ---");
        for (Person p : adults) {
            String extra = p.isSenior ? " (senior)" : "";
            System.out.println(p.name + " is " + p.age + extra);
        }
    }
}
