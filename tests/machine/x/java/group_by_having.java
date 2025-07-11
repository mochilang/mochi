import java.util.*;
class NameCity {
	String name;
	String city;
	NameCity(String name, String city) {
		this.name = name;
		this.city = city;
	}
	int size() { return 2; }
}
class CityNum {
	Object city;
	int num;
	CityNum(Object city, int num) {
		this.city = city;
		this.num = num;
	}
	int size() { return 2; }
}
public class GroupByHaving {
	static String toJson(Object o) {
		if (o instanceof Map<?,?> m) {
			StringJoiner j = new StringJoiner(",", "{", "}");
			for (Map.Entry<?,?> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + e.getValue());
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
	List<NameCity> people = new ArrayList<>(Arrays.asList(new NameCity("Alice", "Paris"), new NameCity("Bob", "Hanoi"), new NameCity("Charlie", "Paris"), new NameCity("Diana", "Hanoi"), new NameCity("Eve", "Paris"), new NameCity("Frank", "Hanoi"), new NameCity("George", "Paris")));
	List<CityNum> big = (new java.util.function.Supplier<List<CityNum>>(){public List<CityNum> get(){
	List<CityNum> _res0 = new ArrayList<>();
	Map<String,List<NameCity>> _groups1 = new LinkedHashMap<>();
	for (var p : people) {
		var _row2 = p;
		String _key3 = p.city;
		List<NameCity> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<String,List<NameCity>> __e : _groups1.entrySet()) {
		String g_key = __e.getKey();
		List<NameCity> g = __e.getValue();
		if (!(g.size() >= 4)) continue;
		_res0.add(new CityNum(g_key, g.size()));
	}
	return _res0;
}}).get();
	json(big);
	}
}
