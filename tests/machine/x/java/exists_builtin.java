import java.util.*;
public class Main {
	static List<Integer> data = java.util.Arrays.asList(1, 2);
	static Object flag = data.stream().anyMatch(x -> x == 1);
	public static void main(String[] args) {
	System.out.println(flag);
	}
}
