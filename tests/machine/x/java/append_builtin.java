public class Main {
	static java.util.List<Integer> append(java.util.List<Integer> l, int v) {
		java.util.List<Integer> out = new java.util.ArrayList<>(l);
		out.add(v);
		return out;
	}
	public static void main(String[] args) {
		var a = new java.util.ArrayList<>(java.util.Arrays.asList(1,2));
		System.out.println(append(a, 3));
	}
}
