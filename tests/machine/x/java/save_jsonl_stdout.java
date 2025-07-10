import java.util.*;
class DataClass1 {
	String name;
	int age;
	DataClass1(String name, int age) {
		this.name = name;
		this.age = age;
	}
}
public class Main {
	static List<DataClass1> people = new ArrayList<>(java.util.Arrays.asList(new DataClass1("Alice", 30), new DataClass1("Bob", 25)));
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
