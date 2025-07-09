import java.util.*;
public class Main {
	static double avg(List<? extends Number> v) {
		if (v.isEmpty()) return 0;
		int s = 0;
		for (Number n : v) s += n.intValue();
		return (double)s / v.size();
	}
	public static void main(String[] args) {
	System.out.println(avg((List<Number>)(List<?>)java.util.Arrays.asList(1, 2, 3)));
	}
}
