import java.util.*;
class CatVal {
	String cat;
	int val;
	CatVal(String cat, int val) {
		this.cat = cat;
		this.val = val;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof CatVal other)) return false;
		return Objects.equals(this.cat, other.cat) && Objects.equals(this.val, other.val);
	}
	@Override public int hashCode() {
		return Objects.hash(cat, val);
	}
}
class CatTotal {
	Object cat;
	int total;
	CatTotal(Object cat, int total) {
		this.cat = cat;
		this.total = total;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof CatTotal other)) return false;
		return Objects.equals(this.cat, other.cat) && Objects.equals(this.total, other.total);
	}
	@Override public int hashCode() {
		return Objects.hash(cat, total);
	}
}
public class GroupBySort {
	public static void main(String[] args) {
	List<CatVal> items = new ArrayList<>(Arrays.asList(new CatVal("a", 3), new CatVal("a", 1), new CatVal("b", 5), new CatVal("b", 2)));
	List<CatTotal> grouped = (new java.util.function.Supplier<List<CatTotal>>(){public List<CatTotal> get(){
	List<CatTotal> _res0 = new ArrayList<>();
	Map<String,List<CatVal>> _groups1 = new LinkedHashMap<>();
	for (var i : items) {
		var _row2 = i;
		String _key3 = i.cat;
		List<CatVal> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<String,List<CatVal>> __e : _groups1.entrySet()) {
		String g_key = __e.getKey();
		List<CatVal> g = __e.getValue();
		_res0.add(new CatTotal(g_key, (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res5 = new ArrayList<>();
	for (var x : g) {
		_res5.add(x.val);
	}
	return _res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
	}
	return _res0;
}}).get();
	System.out.println(grouped);
	}
}
