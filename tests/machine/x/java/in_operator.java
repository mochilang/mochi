import java.util.*;
public class InOperator {
	static List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
	public static void main(String[] args) {
	System.out.println(xs.contains(2));
	System.out.println(!(xs.contains(5)));
	}
}
