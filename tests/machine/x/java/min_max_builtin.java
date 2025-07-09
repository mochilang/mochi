import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(3, 1, 4));
	static int min(List<? extends Number> v) {
		int m = Integer.MAX_VALUE;
		for (Number n : v) if (n.intValue() < m) m = n.intValue();
		return m;
	}
	static int max(List<? extends Number> v) {
		int m = Integer.MIN_VALUE;
		for (Number n : v) if (n.intValue() > m) m = n.intValue();
		return m;
	}
	public static void main(String[] args) {
	System.out.println(min((List<Number>)(List<?>)nums));
	System.out.println(max((List<Number>)(List<?>)nums));
	}
}
