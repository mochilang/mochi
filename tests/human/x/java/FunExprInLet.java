public class FunExprInLet {
    public static void main(String[] args) {
        java.util.function.IntUnaryOperator square = x -> x * x;
        System.out.println(square.applyAsInt(6));
    }
}
