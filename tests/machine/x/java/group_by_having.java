import java.util.*;
class PeopleCityName {
	String name;
	String city;
	PeopleCityName(String name, String city) {
		this.name = name;
		this.city = city;
	}
}
class BigCityNum {
	Object city;
	int num;
	BigCityNum(Object city, int num) {
		this.city = city;
		this.num = num;
	}
}
public class Main {
	static List<PeopleCityName> people = new ArrayList<>(Arrays.asList(new PeopleCityName("Alice", "Paris"), new PeopleCityName("Bob", "Hanoi"), new PeopleCityName("Charlie", "Paris"), new PeopleCityName("Diana", "Hanoi"), new PeopleCityName("Eve", "Paris"), new PeopleCityName("Frank", "Hanoi"), new PeopleCityName("George", "Paris")));
	static List<BigCityNum> big = (new java.util.function.Supplier<List<BigCityNum>>(){public List<BigCityNum> get(){
	List<BigCityNum> _res5 = new ArrayList<>();
	Map<String,List<PeopleCityName>> _groups6 = new LinkedHashMap<>();
	for (var p : people) {
		var _row7 = p;
		String _key8 = p.city;
		List<PeopleCityName> _b9 = _groups6.get(_key8);
		if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
		_b9.add(_row7);
	}
	for (var __e : _groups6.entrySet()) {
		String g_key = __e.getKey();
		List<PeopleCityName> g = __e.getValue();
		if (!(count(g) >= 4)) continue;
		_res5.add(new BigCityNum(g_key, count(g)));
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static String toJson(Object o) {
		if (o instanceof Map<?,?> m) {
			StringJoiner j = new StringJoiner(",", "{", "}");
			for (var e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + e.getValue());
			return j.toString();
		} else if (o instanceof Collection<?> c) {
			StringJoiner j = new StringJoiner(",", "[", "]");
			for (var x : c) j.add(toJson(x));
			return j.toString();
		} else if (o instanceof String s) {
			return "\"" + s + "\"";
		}
		return String.valueOf(o);
	}
	static void json(Object o) { System.out.println(toJson(o)); }
	public static void main(String[] args) {
	json(big);
	}
}
