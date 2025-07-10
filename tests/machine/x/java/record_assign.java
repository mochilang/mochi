import java.util.*;
class Counter {
	int n;
	Counter(int n) {
		this.n = n;
	}
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Counter other)) return false;
		return Objects.equals(this.n, other.n);
	}
	@Override public int hashCode() {
		return Objects.hash(n);
	}
}
public class Main {
	static Counter c = new Counter(0);
	static void inc(Counter c) {
		c.n = c.n + 1;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	inc(c);
	System.out.println(c.n);
	}
}
