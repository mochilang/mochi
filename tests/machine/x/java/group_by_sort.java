import java.util.*;
class DataClass1 {
	String cat;
	int val;
	DataClass1(String cat, int val) {
		this.cat = cat;
		this.val = val;
	}
}
class DataClass2 {
	Object cat;
	int total;
	DataClass2(Object cat, int total) {
		this.cat = cat;
		this.total = total;
	}
}
public class Main {
	static List<DataClass1> items = new ArrayList<>(java.util.Arrays.asList(new DataClass1("a", 3), new DataClass1("a", 1), new DataClass1("b", 5), new DataClass1("b", 2)));
	static List<DataClass2> grouped = (new java.util.function.Supplier<List<DataClass2>>(){public List<DataClass2> get(){
	List<DataClass2> _res6 = new ArrayList<>();
	Map<Object,List<DataClass1>> _groups7 = new LinkedHashMap<>();
	for (var i : items) {
		var _row8 = i;
		Object _key9 = i.cat;
		List<DataClass1> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		_res6.add(new DataClass2(g_key, sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(x.val);
	}
	return _res11;
}}).get())));
	}
	return _res6;
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
