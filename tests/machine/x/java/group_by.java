import java.util.*;
class PeopleAgeCityName {
	String name;
	int age;
	String city;
	PeopleAgeCityName(String name, int age, String city) {
		this.name = name;
		this.age = age;
		this.city = city;
	}
}
class StatsAvgAgeCityCount {
	Object city;
	int count;
	double avg_age;
	StatsAvgAgeCityCount(Object city, int count, double avg_age) {
		this.city = city;
		this.count = count;
		this.avg_age = avg_age;
	}
}
public class GroupBy {
	static List<PeopleAgeCityName> people = new ArrayList<>(Arrays.asList(new PeopleAgeCityName("Alice", 30, "Paris"), new PeopleAgeCityName("Bob", 15, "Hanoi"), new PeopleAgeCityName("Charlie", 65, "Paris"), new PeopleAgeCityName("Diana", 45, "Hanoi"), new PeopleAgeCityName("Eve", 70, "Paris"), new PeopleAgeCityName("Frank", 22, "Hanoi")));
	static List<StatsAvgAgeCityCount> stats = (new java.util.function.Supplier<List<StatsAvgAgeCityCount>>(){public List<StatsAvgAgeCityCount> get(){
	List<StatsAvgAgeCityCount> _res6 = new ArrayList<>();
	Map<String,List<PeopleAgeCityName>> _groups7 = new LinkedHashMap<>();
	for (var person : people) {
		var _row8 = person;
		String _key9 = person.city;
		List<PeopleAgeCityName> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		String g_key = __e.getKey();
		List<PeopleAgeCityName> g = __e.getValue();
		_res6.add(new StatsAvgAgeCityCount(g_key, g.size(), (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(p.age);
	}
	return _res11;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0)));
	}
	return _res6;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (StatsAvgAgeCityCount s : stats) {
		System.out.println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age);
	}
	}
}
