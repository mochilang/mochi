import java.util.*;
public class AvgBuiltin {
	public static void main(String[] args) {
	System.out.println(Arrays.asList(1, 2, 3).stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0));
	}
}
