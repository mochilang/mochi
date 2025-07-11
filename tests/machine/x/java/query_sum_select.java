import java.util.*;
public class QuerySumSelect {
	public static void main(String[] args) {
	List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	int result = (new java.util.function.Supplier<Integer>(){public Integer get(){
	int _sum1 = 0;
	for (var n : nums) {
		if (!(n > 1)) continue;
		_sum1 += n;
	}
	return _sum1;
}}).get();
	System.out.println(result);
	}
}
