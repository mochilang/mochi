import java.util.*;
class DataClass1 {
	Object n;
	Object l;
	Object b;
	DataClass1(Object n, Object l, Object b) {
		this.n = n;
		this.l = l;
		this.b = b;
	}
}
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(1, 2));
	static List<String> letters = new ArrayList<>(java.util.Arrays.asList("A", "B"));
	static List<Boolean> bools = new ArrayList<>(java.util.Arrays.asList(true, false));
	static List<DataClass1> combos = (new java.util.function.Supplier<List<DataClass1>>(){public List<DataClass1> get(){
	List<DataClass1> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			for (var b : bools) {
				_res1.add(new DataClass1(n, l, b));
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join of three lists ---");
	for (Object c : combos) {
		System.out.println(((Map)c).get("n") + " " + ((Map)c).get("l") + " " + ((Map)c).get("b"));
	}
	}
}
