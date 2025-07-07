import java.util.*;

class Person {
    String name;
    int age;
    String city;

    Person(String name, int age, String city) {
        this.name = name;
        this.age = age;
        this.city = city;
    }
}

class Stat {
    String city;
    int count;
    double avgAge;

    Stat(String city, int count, double avgAge) {
        this.city = city;
        this.count = count;
        this.avgAge = avgAge;
    }
}

public class GroupBy {
    public static void main(String[] args) {
        List<Person> people = Arrays.asList(
            new Person("Alice", 30, "Paris"),
            new Person("Bob", 15, "Hanoi"),
            new Person("Charlie", 65, "Paris"),
            new Person("Diana", 45, "Hanoi"),
            new Person("Eve", 70, "Paris"),
            new Person("Frank", 22, "Hanoi")
        );

        Map<String, int[]> groups = new HashMap<>();
        for (Person p : people) {
            int[] agg = groups.getOrDefault(p.city, new int[]{0,0}); // [totalAge,count]
            agg[0] += p.age;
            agg[1] += 1;
            groups.put(p.city, agg);
        }

        List<Stat> stats = new ArrayList<>();
        for (Map.Entry<String, int[]> e : groups.entrySet()) {
            int totalAge = e.getValue()[0];
            int count = e.getValue()[1];
            stats.add(new Stat(e.getKey(), count, totalAge / (double) count));
        }

        System.out.println("--- People grouped by city ---");
        for (Stat s : stats) {
            System.out.println(s.city + ": count = " + s.count + ", avg_age = " + s.avgAge);
        }
    }
}
