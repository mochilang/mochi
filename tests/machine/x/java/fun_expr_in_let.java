import java.util.function.*;

public class FunExprInLet {
    public static void main(String[] args) {
    IntUnaryOperator square = x -> x * x;
    System.out.println(square.applyAsInt(6));
    }
}
