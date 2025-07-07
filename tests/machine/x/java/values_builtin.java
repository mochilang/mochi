public class Main {
	public static void main(String[] args) {
		var m = new java.util.LinkedHashMap<>(java.util.Map.of("a", 1,"b", 2,"c", 3));
		System.out.println(values(m));
	}
}
