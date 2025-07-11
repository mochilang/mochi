import java.util.*;
public class BreakContinue {
	public static void main(String[] args) {
	List<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
	for (Integer n : numbers) {
		if (Objects.equals(n % 2, 0)) {
			continue;
		}
		if (n > 7) {
			break;
		}
		System.out.println("odd number:" + " " + n);
	}
	}
}
