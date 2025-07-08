import java.util.*;
public class Main {
	static double avg(List<Integer> v) {
		if (v.isEmpty()) return 0;
		int s = 0;
		for (int n : v) s += n;
		return (double)s / v.size();
	}
	public static void main(String[] args) {
	System.out.println(avg(java.util.Arrays.asList(1, 2, 3)));
	}
}
