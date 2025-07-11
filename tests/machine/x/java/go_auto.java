import java.util.*;
class testpkg {
	static int Add(int a, int b) { return a + b; }
	static double Pi = 3.14;
	static int Answer = 42;
}
public class GoAuto {
	public static void main(String[] args) {
	System.out.println(testpkg.Add(2, 3));
	System.out.println(testpkg.Pi);
	System.out.println(testpkg.Answer);
	}
}
