import java.util.*;
public class Main {
	static int a = java.util.Arrays.asList(1, 2);
	static <T> List<T> append(List<T> list, T item) {
		List<T> res = new ArrayList<>(list);
		res.add(item);
		return res;
	}
	public static void main(String[] args) {
	System.out.println(append(a, 3));
	}
}
