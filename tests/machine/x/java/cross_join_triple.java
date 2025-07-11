import java.util.*;
class CombosBLN {
	Integer n;
	String l;
	Boolean b;
	CombosBLN(Integer n, String l, Boolean b) {
		this.n = n;
		this.l = l;
		this.b = b;
	}
}
public class Main {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2));
	static List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
	static List<Boolean> bools = new ArrayList<>(Arrays.asList(true, false));
	static List<CombosBLN> combos = (new java.util.function.Supplier<List<CombosBLN>>(){public List<CombosBLN> get(){
	List<CombosBLN> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			for (var b : bools) {
				_res1.add(new CombosBLN(n, l, b));
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join of three lists ---");
	for (CombosBLN c : combos) {
		System.out.println(c.n + " " + c.l + " " + c.b);
	}
	}
}
