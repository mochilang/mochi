import java.util.function.*;
public class PartialApplication {
	static int add(int a, int b) {
		return a + b;
	}
	public static void main(String[] args) {
	IntUnaryOperator add5 = b -> add(5, b);
	System.out.println(add5.applyAsInt(3));
	}
}
