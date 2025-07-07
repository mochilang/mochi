public class Main {
	public static void main(String[] args) {
		var xs = new java.util.ArrayList<>(java.util.Arrays.asList(1,2,3));
		System.out.println(2 in xs);
		System.out.println(!(5 in xs));
	}
}
