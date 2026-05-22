public class Main {

    static int outer(int x) {
        java.util.function.IntUnaryOperator inner = (int y) -> x + y;
        return inner.applyAsInt(5);
    }
    public static void main(String[] args) {
        System.out.println(outer(3));
    }
}
