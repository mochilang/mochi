import java.util.function.*;
public class FunExprInLet {
	static IntUnaryOperator square = x -> x * x;
	public static void main(String[] args) {
	System.out.println(square.applyAsInt(6));
	}
}
