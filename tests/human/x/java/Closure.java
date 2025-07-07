import java.util.function.IntUnaryOperator;

public class Closure {
    static IntUnaryOperator makeAdder(int n) {
        return x -> x + n;
    }
    public static void main(String[] args) {
        IntUnaryOperator add10 = makeAdder(10);
        System.out.println(add10.applyAsInt(7));
    }
}
