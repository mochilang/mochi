import java.util.*;
public class Main {
	static List<List<Integer>> matrix = java.util.Arrays.asList(java.util.Arrays.asList(1, 2), java.util.Arrays.asList(3, 4));
	public static void main(String[] args) {
	((List)matrix.get(1)).set(0, 5);
	System.out.println(((List)matrix.get(1)).get(0));
	}
}
