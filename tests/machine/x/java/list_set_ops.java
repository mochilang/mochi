public class Main {
	public static void main(String[] args) {
		System.out.println(new java.util.ArrayList<>(java.util.Arrays.asList(1,2)) union new java.util.ArrayList<>(java.util.Arrays.asList(2,3)));
		System.out.println(new java.util.ArrayList<>(java.util.Arrays.asList(1,2,3)) except new java.util.ArrayList<>(java.util.Arrays.asList(2)));
		System.out.println(new java.util.ArrayList<>(java.util.Arrays.asList(1,2,3)) intersect new java.util.ArrayList<>(java.util.Arrays.asList(2,4)));
		System.out.println(new java.util.ArrayList<>(java.util.Arrays.asList(1,2)) union new java.util.ArrayList<>(java.util.Arrays.asList(2,3)).size());
	}
}
