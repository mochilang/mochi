import java.util.*;
class ItemsNV {
	int n;
	String v;
	ItemsNV(int n, String v) {
		this.n = n;
		this.v = v;
	}
}
public class SortStable {
	static List<ItemsNV> items = new ArrayList<>(Arrays.asList(new ItemsNV(1, "a"), new ItemsNV(1, "b"), new ItemsNV(2, "c")));
	static List<String> result = (new java.util.function.Supplier<List<String>>(){public List<String> get(){
	List<String> _res1 = new ArrayList<>();
	for (var i : items) {
		_res1.add(i.v);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(result);
	}
}
