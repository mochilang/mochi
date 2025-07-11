import java.util.*;
class ItemsCatFlagVal {
	String cat;
	int val;
	boolean flag;
	ItemsCatFlagVal(String cat, int val, boolean flag) {
		this.cat = cat;
		this.val = val;
		this.flag = flag;
	}
}
class ResultCatShare {
	Object cat;
	double share;
	ResultCatShare(Object cat, double share) {
		this.cat = cat;
		this.share = share;
	}
}
public class Main {
	static List<ItemsCatFlagVal> items = new ArrayList<>(Arrays.asList(new ItemsCatFlagVal("a", 10, true), new ItemsCatFlagVal("a", 5, false), new ItemsCatFlagVal("b", 20, true)));
	static List<ResultCatShare> result = (new java.util.function.Supplier<List<ResultCatShare>>(){public List<ResultCatShare> get(){
	List<ResultCatShare> _res7 = new ArrayList<>();
	Map<String,List<ItemsCatFlagVal>> _groups8 = new LinkedHashMap<>();
	for (var i : items) {
		var _row9 = i;
		String _key10 = i.cat;
		List<ItemsCatFlagVal> _b11 = _groups8.get(_key10);
		if (_b11 == null) { _b11 = new ArrayList<>(); _groups8.put(_key10, _b11); }
		_b11.add(_row9);
	}
	for (var __e : _groups8.entrySet()) {
		String g_key = __e.getKey();
		List<ItemsCatFlagVal> g = __e.getValue();
		_res7.add(new ResultCatShare(g_key, ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
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
