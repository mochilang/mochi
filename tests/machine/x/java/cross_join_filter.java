import java.util.*;
class DataClass1 {
	Object n;
	Object l;
	DataClass1(Object n, Object l) {
		this.n = n;
		this.l = l;
	}
}
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(1, 2, 3));
	static List<String> letters = new ArrayList<>(java.util.Arrays.asList("A", "B"));
	static List<DataClass1> pairs = (new java.util.function.Supplier<List<DataClass1>>(){public List<DataClass1> get(){
	List<DataClass1> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(Objects.equals(n % 2, 0))) continue;
			_res1.add(new DataClass1(n, l));
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Even pairs ---");
	for (Object p : pairs) {
		System.out.println(((Map)p).get("n") + " " + ((Map)p).get("l"));
	}
	}
}
