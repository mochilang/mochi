import java.util.*;
import java.util.function.*;
public class Main {
	static IntUnaryOperator add10 = makeAdder(10);
	static IntUnaryOperator makeAdder(int n) {
		return x -> x + n;
	}
	public static void main(String[] args) {
	System.out.println(add10.applyAsInt(7));
	}
}
