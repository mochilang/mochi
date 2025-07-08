import java.util.*;
public class Main {
	static int sum_rec(int n, int acc) {
		if (n == 0) {
			return acc;
		}
		return sum_rec(n - 1, acc + n);
	}
	public static void main(String[] args) {
	System.out.println(sum_rec(10, 0));
	}
}
