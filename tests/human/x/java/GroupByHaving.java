import java.util.*;

class PersonGH {
    String name;
    String city;
    PersonGH(String name, String city) {
        this.name = name;
        this.city = city;
    }
}

class StatGH {
    String city;
    int num;
    StatGH(String city, int num) {
        this.city = city;
        this.num = num;
    }
}

public class GroupByHaving {
    public static void main(String[] args) {
        List<PersonGH> people = Arrays.asList(
            new PersonGH("Alice", "Paris"),
            new PersonGH("Bob", "Hanoi"),
            new PersonGH("Charlie", "Paris"),
            new PersonGH("Diana", "Hanoi"),
            new PersonGH("Eve", "Paris"),
            new PersonGH("Frank", "Hanoi"),
            new PersonGH("George", "Paris")
        );

        Map<String, Integer> counts = new LinkedHashMap<>();
        for (PersonGH p : people) {
            counts.put(p.city, counts.getOrDefault(p.city, 0) + 1);
        }

        List<StatGH> big = new ArrayList<>();
        for (Map.Entry<String, Integer> e : counts.entrySet()) {
            if (e.getValue() >= 4) {
                big.add(new StatGH(e.getKey(), e.getValue()));
            }
        }
        System.out.println(toJson(big));
    }

    static String toJson(List<StatGH> stats) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < stats.size(); i++) {
            StatGH s = stats.get(i);
            sb.append("{\"city\":\"").append(s.city).append("\",\"num\":").append(s.num).append("}");
            if (i < stats.size() - 1) sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }
}
