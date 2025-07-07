public class Main {
	public static void main(String[] args) {
		var m = new java.util.LinkedHashMap<>(java.util.Map.of(1, "a",2, "b"));
		System.out.println(1 in m);
		System.out.println(3 in m);
	}
}
