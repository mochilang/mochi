import java.util.*;
public class Main {
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println(count(Arrays.asList(1, 2, 3)));
	}
}
