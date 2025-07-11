import java.util.*;
class NameAgeCity {
	String name;
	int age;
	String city;
	NameAgeCity(String name, int age, String city) {
		this.name = name;
		this.age = age;
		this.city = city;
	}
	int size() { return 3; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof NameAgeCity other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age) && Objects.equals(this.city, other.city);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age, city);
	}
}
class CityCountAvgAge {
	Object city;
	int count;
	double avg_age;
	CityCountAvgAge(Object city, int count, double avg_age) {
		this.city = city;
		this.count = count;
		this.avg_age = avg_age;
	}
	int size() { return 3; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof CityCountAvgAge other)) return false;
		return Objects.equals(this.city, other.city) && Objects.equals(this.count, other.count) && Objects.equals(this.avg_age, other.avg_age);
	}
	@Override public int hashCode() {
		return Objects.hash(city, count, avg_age);
	}
}
public class GroupBy {
	public static void main(String[] args) {
	List<NameAgeCity> people = new ArrayList<>(Arrays.asList(new NameAgeCity("Alice", 30, "Paris"), new NameAgeCity("Bob", 15, "Hanoi"), new NameAgeCity("Charlie", 65, "Paris"), new NameAgeCity("Diana", 45, "Hanoi"), new NameAgeCity("Eve", 70, "Paris"), new NameAgeCity("Frank", 22, "Hanoi")));
	List<CityCountAvgAge> stats = (new java.util.function.Supplier<List<CityCountAvgAge>>(){public List<CityCountAvgAge> get(){
	List<CityCountAvgAge> _res0 = new ArrayList<>();
	Map<String,List<NameAgeCity>> _groups1 = new LinkedHashMap<>();
	for (var person : people) {
		var _row2 = person;
		String _key3 = person.city;
		List<NameAgeCity> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<String,List<NameAgeCity>> __e : _groups1.entrySet()) {
		String g_key = __e.getKey();
		List<NameAgeCity> g = __e.getValue();
		_res0.add(new CityCountAvgAge(g_key, g.size(), (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res5 = new ArrayList<>();
	for (var p : g) {
		_res5.add(p.age);
	}
	return _res5;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0)));
	}
	return _res0;
}}).get();
	System.out.println("--- People grouped by city ---");
	for (CityCountAvgAge s : stats) {
		System.out.println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age);
	}
	}
}
