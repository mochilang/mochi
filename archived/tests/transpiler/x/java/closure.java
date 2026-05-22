public class Main {
    static java.util.function.IntUnaryOperator add10 = makeAdder(10);

    static java.util.function.IntUnaryOperator makeAdder(int n) {
        return (int x) -> x + n;
    }
    public static void main(String[] args) {
        System.out.println(add10.applyAsInt(7));
    }
}
