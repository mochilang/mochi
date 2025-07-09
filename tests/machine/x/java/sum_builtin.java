import java.util.*;
public class Main {
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(sum((List<Number>)(List<?>)java.util.Arrays.asList(1, 2, 3)));
	}
}
