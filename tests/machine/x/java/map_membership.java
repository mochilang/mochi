public class Main {
	public static void main(String[] args) {
		var m = new java.util.LinkedHashMap<>(java.util.Map.of("a", 1,"b", 2));
		System.out.println("a" in m);
		System.out.println("c" in m);
	}
}
