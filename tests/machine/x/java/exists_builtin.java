import java.util.*;
public class Main {
	static List<Integer> data = new ArrayList<>(java.util.Arrays.asList(1, 2));
	static Object flag = data.stream().anyMatch(x -> Objects.equals(x, 1));
	public static void main(String[] args) {
	System.out.println(flag);
	}
}
