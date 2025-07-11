import java.util.*;
class ItemsCatVal {
	String cat;
	int val;
	ItemsCatVal(String cat, int val) {
		this.cat = cat;
		this.val = val;
	}
}
class GroupedCatTotal {
	Object cat;
	int total;
	GroupedCatTotal(Object cat, int total) {
		this.cat = cat;
		this.total = total;
	}
}
public class Main {
	static List<ItemsCatVal> items = new ArrayList<>(Arrays.asList(new ItemsCatVal("a", 3), new ItemsCatVal("a", 1), new ItemsCatVal("b", 5), new ItemsCatVal("b", 2)));
	static List<GroupedCatTotal> grouped = (new java.util.function.Supplier<List<GroupedCatTotal>>(){public List<GroupedCatTotal> get(){
	List<GroupedCatTotal> _res6 = new ArrayList<>();
	Map<String,List<ItemsCatVal>> _groups7 = new LinkedHashMap<>();
	for (var i : items) {
		var _row8 = i;
		String _key9 = i.cat;
		List<ItemsCatVal> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		String g_key = __e.getKey();
		List<ItemsCatVal> g = __e.getValue();
		_res6.add(new GroupedCatTotal(g_key, (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(x.val);
	}
	return _res11;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
	}
	return _res6;
}}).get();
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
