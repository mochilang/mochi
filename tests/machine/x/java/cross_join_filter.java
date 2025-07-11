import java.util.*;
class PairsLN {
	Integer n;
	String l;
	PairsLN(Integer n, String l) {
		this.n = n;
		this.l = l;
	}
}
public class Main {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	static List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
	static List<PairsLN> pairs = (new java.util.function.Supplier<List<PairsLN>>(){public List<PairsLN> get(){
	List<PairsLN> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(Objects.equals(n % 2, 0))) continue;
			_res1.add(new PairsLN(n, l));
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Even pairs ---");
	for (PairsLN p : pairs) {
		System.out.println(p.n + " " + p.l);
	}
	}
}
