import java.util.function.*;
public class NestedFunction {
	static int outer(int x) {
		java.util.function.IntUnaryOperator inner = y -> {
			return x + y;
		};
		return inner.applyAsInt(5);
	}
	public static void main(String[] args) {
	System.out.println(outer(3));
	}
}
