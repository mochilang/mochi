public class StringInOperator {
	static String s = "catch";
	public static void main(String[] args) {
	System.out.println(s.contains(String.valueOf("cat")));
	System.out.println(s.contains(String.valueOf("dog")));
	}
}
