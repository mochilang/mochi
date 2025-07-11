import java.util.*;
class PeopleAgeName {
	String name;
	int age;
	PeopleAgeName(String name, int age) {
		this.name = name;
		this.age = age;
	}
}
public class SaveJsonlStdout {
	static List<PeopleAgeName> people = new ArrayList<>(Arrays.asList(new PeopleAgeName("Alice", 30), new PeopleAgeName("Bob", 25)));
	static void saveJsonl(List<Map<?,?>> list) {
		for (Map<?,?> m : list) {
			List<String> parts = new ArrayList<>();
			for (var e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
			System.out.println("{" + String.join(",", parts) + "}");
		}
	}
	public static void main(String[] args) {
	saveJsonl((List<Map<?,?>>)(List<?>)people);
	}
}
