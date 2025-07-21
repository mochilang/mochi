public class Main {
    static java.util.function.IntUnaryOperator square = (int x) -> x * x;

    public static void main(String[] args) {
        System.out.println(square.applyAsInt(6));
    }
}
