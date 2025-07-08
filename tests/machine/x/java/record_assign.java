import java.util.*;
class Counter {
	int n;
	Counter(int n) {
		this.n = n;
	}
}
public class Main {
	static Object c = new Counter(0);
	static int inc(Counter c) {
		c = c.n + 1;
	}
	public static void main(String[] args) {
	inc(c);
	System.out.println(c.n);
	}
}
