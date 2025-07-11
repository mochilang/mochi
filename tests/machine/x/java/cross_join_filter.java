import java.util.*;
class NL {
	Integer n;
	String l;
	NL(Integer n, String l) {
		this.n = n;
		this.l = l;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof NL other)) return false;
		return Objects.equals(this.n, other.n) && Objects.equals(this.l, other.l);
	}
	@Override public int hashCode() {
		return Objects.hash(n, l);
	}
}
public class CrossJoinFilter {
	public static void main(String[] args) {
	List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
	List<NL> pairs = (new java.util.function.Supplier<List<NL>>(){public List<NL> get(){
	List<NL> _res0 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(Objects.equals(n % 2, 0))) continue;
			_res0.add(new NL(n, l));
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Even pairs ---");
	for (NL p : pairs) {
		System.out.println(p.n + " " + p.l);
	}
	}
}
