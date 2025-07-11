import java.util.*;

class NameAge {
    String name;
    int age;
    NameAge(String name, int age) {
        this.name = name;
        this.age = age;
    }
    int size() { return 2; }
}
class NameAgeIsSenior {
    String name;
    int age;
    boolean is_senior;
    NameAgeIsSenior(String name, int age, boolean is_senior) {
        this.name = name;
        this.age = age;
        this.is_senior = is_senior;
    }
    int size() { return 3; }
}
public class DatasetWhereFilter {
    public static void main(String[] args) {
    List<NameAge> people = new ArrayList<>(Arrays.asList(new NameAge("Alice", 30), new NameAge("Bob", 15), new NameAge("Charlie", 65), new NameAge("Diana", 45)));
    List<NameAgeIsSenior> adults = (new java.util.function.Supplier<List<NameAgeIsSenior>>(){public List<NameAgeIsSenior> get(){
    List<NameAgeIsSenior> _res0 = new ArrayList<>();
    for (var person : people) {
        if (!(person.age >= 18)) continue;
        _res0.add(new NameAgeIsSenior(person.name, person.age, person.age >= 60));
    }
    return _res0;
}}).get();
    System.out.println("--- Adults ---");
    for (NameAgeIsSenior person : adults) {
        System.out.println(person.name + " " + "is" + " " + person.age + " " + (person.is_senior ? " (senior)" : ""));
    }
    }
}
