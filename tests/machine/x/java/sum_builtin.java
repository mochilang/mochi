import java.util.*;
public class Main {
	static int sum(List<Integer> v) {
		int s = 0;
		for (int n : v) s += n;
		return s;
	}
	public static void main(String[] args) {
	System.out.println(sum(java.util.Arrays.asList(1, 2, 3)));
	}
}
