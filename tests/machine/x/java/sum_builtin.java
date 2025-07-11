import java.util.*;
public class Main {
	public static void main(String[] args) {
	System.out.println(Arrays.asList(1, 2, 3).stream().mapToInt(n -> ((Number)n).intValue()).sum());
	}
}
