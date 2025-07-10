import java.util.*;
class DataClass1 {
	int id;
	String name;
	DataClass1(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class DataClass2 {
	int id;
	int nation;
	DataClass2(int id, int nation) {
		this.id = id;
		this.nation = nation;
	}
}
class DataClass3 {
	int part;
	int supplier;
	double cost;
	int qty;
	DataClass3(int part, int supplier, double cost, int qty) {
		this.part = part;
		this.supplier = supplier;
		this.cost = cost;
		this.qty = qty;
	}
}
class DataClass4 {
	int part;
	double value;
	DataClass4(int part, double value) {
		this.part = part;
		this.value = value;
	}
}
class DataClass5 {
	Object part;
	int total;
	DataClass5(Object part, int total) {
		this.part = part;
		this.total = total;
	}
}
public class Main {
	static List<DataClass1> nations = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "A"), new DataClass1(2, "B")));
	static List<DataClass2> suppliers = new ArrayList<>(java.util.Arrays.asList(new DataClass2(1, 1), new DataClass2(2, 2)));
	static List<DataClass3> partsupp = new ArrayList<>(java.util.Arrays.asList(new DataClass3(100, 1, 10.000000, 2), new DataClass3(100, 2, 20.000000, 1), new DataClass3(200, 1, 5.000000, 3)));
	static List<DataClass4> filtered = (new java.util.function.Supplier<List<DataClass4>>(){public List<DataClass4> get(){
	List<DataClass4> _res7 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(Objects.equals(s.id, ps.supplier))) continue;
			for (var n : nations) {
				if (!(Objects.equals(n.id, s.nation))) continue;
				if (!(Objects.equals(n.name, "A"))) continue;
				_res7.add(new DataClass4(ps.part, ps.cost * ps.qty));
			}
		}
	}
	return _res7;
}}).get();
	static List<DataClass5> grouped = (new java.util.function.Supplier<List<DataClass5>>(){public List<DataClass5> get(){
	List<DataClass5> _res8 = new ArrayList<>();
	Map<Object,List<DataClass4>> _groups9 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row10 = x;
		Object _key11 = x.part;
		List<DataClass4> _b12 = _groups9.get(_key11);
		if (_b12 == null) { _b12 = new ArrayList<>(); _groups9.put(_key11, _b12); }
		_b12.add(_row10);
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass4> g = __e.getValue();
		_res8.add(new DataClass5(g_key, sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
	List<Double> _res13 = new ArrayList<>();
	for (var r : g) {
		_res13.add(r.value);
	}
	return _res13;
}}).get())));
	}
	return _res8;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
