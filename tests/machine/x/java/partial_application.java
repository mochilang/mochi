import java.util.*;
import java.util.function.*;
public class Main {
	static IntUnaryOperator add5 = b -> add(5, b);
	static int add(int a, int b) {
		return a + b;
	}
	public static void main(String[] args) {
	System.out.println(add5.applyAsInt(3));
	}
}
