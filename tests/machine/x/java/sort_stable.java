import java.util.*;
class NV {
	int n;
	String v;
	NV(int n, String v) {
		this.n = n;
		this.v = v;
	}
	int size() { return 2; }
}
public class SortStable {
	public static void main(String[] args) {
	List<NV> items = new ArrayList<>(Arrays.asList(new NV(1, "a"), new NV(1, "b"), new NV(2, "c")));
	List<String> result = (new java.util.function.Supplier<List<String>>(){public List<String> get(){
	List<String> _res0 = new ArrayList<>();
	for (var i : items) {
		_res0.add(i.v);
	}
	return _res0;
}}).get();
	System.out.println(result);
	}
}
