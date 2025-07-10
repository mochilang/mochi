import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(1, 2, 3));
	static int result = (new java.util.function.Supplier<Integer>(){public Integer get(){
	int _sum3 = 0;
	for (var n : nums) {
		if (!(n > 1)) continue;
		_sum3 += n;
	}
	return _sum3;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
