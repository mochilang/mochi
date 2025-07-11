import java.util.*;
class NationsIdName {
	int id;
	String name;
	NationsIdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class SuppliersIdNation {
	int id;
	int nation;
	SuppliersIdNation(int id, int nation) {
		this.id = id;
		this.nation = nation;
	}
}
class PartsuppCostPartQtySupplier {
	int part;
	int supplier;
	double cost;
	int qty;
	PartsuppCostPartQtySupplier(int part, int supplier, double cost, int qty) {
		this.part = part;
		this.supplier = supplier;
		this.cost = cost;
		this.qty = qty;
	}
}
class FilteredPartValue {
	int part;
	double value;
	FilteredPartValue(int part, double value) {
		this.part = part;
		this.value = value;
	}
}
class GroupedPartTotal {
	Object part;
	int total;
	GroupedPartTotal(Object part, int total) {
		this.part = part;
		this.total = total;
	}
}
public class GroupByMultiJoin {
	static List<NationsIdName> nations = new ArrayList<>(Arrays.asList(new NationsIdName(1, "A"), new NationsIdName(2, "B")));
	static List<SuppliersIdNation> suppliers = new ArrayList<>(Arrays.asList(new SuppliersIdNation(1, 1), new SuppliersIdNation(2, 2)));
	static List<PartsuppCostPartQtySupplier> partsupp = new ArrayList<>(Arrays.asList(new PartsuppCostPartQtySupplier(100, 1, 10.000000, 2), new PartsuppCostPartQtySupplier(100, 2, 20.000000, 1), new PartsuppCostPartQtySupplier(200, 1, 5.000000, 3)));
	static List<FilteredPartValue> filtered = (new java.util.function.Supplier<List<FilteredPartValue>>(){public List<FilteredPartValue> get(){
	List<FilteredPartValue> _res7 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(Objects.equals(s.id, ps.supplier))) continue;
			for (var n : nations) {
				if (!(Objects.equals(n.id, s.nation))) continue;
				if (!(Objects.equals(n.name, "A"))) continue;
				_res7.add(new FilteredPartValue(ps.part, ps.cost * ps.qty));
			}
		}
	}
	return _res7;
}}).get();
	static List<GroupedPartTotal> grouped = (new java.util.function.Supplier<List<GroupedPartTotal>>(){public List<GroupedPartTotal> get(){
	List<GroupedPartTotal> _res8 = new ArrayList<>();
	Map<Integer,List<FilteredPartValue>> _groups9 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row10 = x;
		int _key11 = x.part;
		List<FilteredPartValue> _b12 = _groups9.get(_key11);
		if (_b12 == null) { _b12 = new ArrayList<>(); _groups9.put(_key11, _b12); }
		_b12.add(_row10);
	}
	for (var __e : _groups9.entrySet()) {
		int g_key = __e.getKey();
		List<FilteredPartValue> g = __e.getValue();
		_res8.add(new GroupedPartTotal(g_key, (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
	List<Double> _res13 = new ArrayList<>();
	for (var r : g) {
		_res13.add(r.value);
	}
	return _res13;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
	}
	return _res8;
}}).get();
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
