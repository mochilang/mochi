import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	static int result = (new java.util.function.Supplier<Integer>(){public Integer get(){
	int _sum3 = 0;
	for (var n : nums) {
		if (!(n > 1)) continue;
		_sum3 += n;
	}
	return _sum3;
}}).get();
	public static void main(String[] args) {
	System.out.println(result);
	}
}
