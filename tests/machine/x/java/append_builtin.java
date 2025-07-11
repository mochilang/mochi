import java.util.*;
public class AppendBuiltin {
	static List<Integer> a = new ArrayList<>(Arrays.asList(1, 2));
	public static void main(String[] args) {
	System.out.println(java.util.stream.Stream.concat(a.stream(), java.util.stream.Stream.of(3)).collect(java.util.stream.Collectors.toList()));
	}
}
