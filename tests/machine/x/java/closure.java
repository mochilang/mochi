import java.util.*;
import java.util.function.*;
public class Main {
	static IntUnaryOperator add10 = makeAdder(10);
	static IntUnaryOperator makeAdder(int n) {
		return x -> x + n;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(add10.applyAsInt(7));
	}
}
