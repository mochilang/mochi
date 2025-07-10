import java.util.*;
class DataClass1 {
	String name;
	String city;
	DataClass1(String name, String city) {
		this.name = name;
		this.city = city;
	}
}
class DataClass2 {
	Object city;
	Object num;
	DataClass2(Object city, Object num) {
		this.city = city;
		this.num = num;
	}
}
public class Main {
	static List<DataClass1> people = new ArrayList<>(java.util.Arrays.asList(new DataClass1("Alice", "Paris"), new DataClass1("Bob", "Hanoi"), new DataClass1("Charlie", "Paris"), new DataClass1("Diana", "Hanoi"), new DataClass1("Eve", "Paris"), new DataClass1("Frank", "Hanoi"), new DataClass1("George", "Paris")));
	static List<DataClass2> big = (new java.util.function.Supplier<List<DataClass2>>(){public List<DataClass2> get(){
	List<DataClass2> _res5 = new ArrayList<>();
	Map<Object,List<DataClass1>> _groups6 = new LinkedHashMap<>();
	for (var p : people) {
		var _row7 = p;
		Object _key8 = p.city;
		List<DataClass1> _b9 = _groups6.get(_key8);
		if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
		_b9.add(_row7);
	}
	for (var __e : _groups6.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		if (!(count(g) >= 4)) continue;
		_res5.add(new DataClass2(g_key, count(g)));
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
