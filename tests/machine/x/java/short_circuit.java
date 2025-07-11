import java.util.*;
public class ShortCircuit {
	static boolean boom(int a, int b) {
		System.out.println("boom");
		return true;
	}
	public static void main(String[] args) {
	System.out.println(false && boom(1, 2));
	System.out.println(true || boom(1, 2));
	}
}
