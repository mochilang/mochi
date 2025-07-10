import java.util.*;
class DataClass1 {
	String name;
	int age;
	String city;
	DataClass1(String name, int age, String city) {
		this.name = name;
		this.age = age;
		this.city = city;
	}
}
class DataClass2 {
	Object city;
	Object count;
	double avg_age;
	DataClass2(Object city, Object count, double avg_age) {
		this.city = city;
		this.count = count;
		this.avg_age = avg_age;
	}
}
public class Main {
	static List<DataClass1> people = new ArrayList<>(java.util.Arrays.asList(new DataClass1("Alice", 30, "Paris"), new DataClass1("Bob", 15, "Hanoi"), new DataClass1("Charlie", 65, "Paris"), new DataClass1("Diana", 45, "Hanoi"), new DataClass1("Eve", 70, "Paris"), new DataClass1("Frank", 22, "Hanoi")));
	static List<DataClass2> stats = (new java.util.function.Supplier<List<DataClass2>>(){public List<DataClass2> get(){
	List<DataClass2> _res6 = new ArrayList<>();
	Map<Object,List<DataClass1>> _groups7 = new LinkedHashMap<>();
	for (var person : people) {
		var _row8 = person;
		Object _key9 = person.city;
		List<DataClass1> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		_res6.add(new DataClass2(g_key, count(g), avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(p.age);
	}
	return _res11;
}}).get())));
	}
	return _res6;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static double avg(List<? extends Number> v) {
		if (v.isEmpty()) return 0;
		int s = 0;
		for (Number n : v) s += n.intValue();
		return (double)s / v.size();
	}
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (Object s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
