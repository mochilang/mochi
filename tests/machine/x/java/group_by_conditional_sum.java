import java.util.*;
class DataClass1 {
	String cat;
	int val;
	boolean flag;
	DataClass1(String cat, int val, boolean flag) {
		this.cat = cat;
		this.val = val;
		this.flag = flag;
	}
}
class DataClass2 {
	Object cat;
	int share;
	DataClass2(Object cat, int share) {
		this.cat = cat;
		this.share = share;
	}
}
public class Main {
	static List<DataClass1> items = new ArrayList<>(java.util.Arrays.asList(new DataClass1("a", 10, true), new DataClass1("a", 5, false), new DataClass1("b", 20, true)));
	static List<DataClass2> result = (new java.util.function.Supplier<List<DataClass2>>(){public List<DataClass2> get(){
	List<DataClass2> _res7 = new ArrayList<>();
	Map<String,List<DataClass1>> _groups8 = new LinkedHashMap<>();
	for (var i : items) {
		var _row9 = i;
		String _key10 = i.cat;
		List<DataClass1> _b11 = _groups8.get(_key10);
		if (_b11 == null) { _b11 = new ArrayList<>(); _groups8.put(_key10, _b11); }
		_b11.add(_row9);
	}
	for (var __e : _groups8.entrySet()) {
		String g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		_res7.add(new DataClass2(g_key, ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res12 = new ArrayList<>();
	for (var x : g) {
		_res12.add((x.flag ? x.val : 0));
	}
	return _res12;
}}).get())).doubleValue() / ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res13 = new ArrayList<>();
	for (var x : g) {
		_res13.add(x.val);
	}
	return _res13;
}}).get())).doubleValue()));
	}
	return _res7;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
