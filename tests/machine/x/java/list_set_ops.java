import java.util.*;
public class Main {
	public static void main(String[] args) {
	System.out.println(java.util.Arrays.asList(1, 2) union java.util.Arrays.asList(2, 3));
	System.out.println(java.util.Arrays.asList(1, 2, 3) except java.util.Arrays.asList(2));
	System.out.println(java.util.Arrays.asList(1, 2, 3) intersect java.util.Arrays.asList(2, 4));
	System.out.println(java.util.Arrays.asList(1, 2) union java.util.Arrays.asList(2, 3).size());
	}
}
