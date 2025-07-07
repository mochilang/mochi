public class PartialApplication {
    static int add(int a, int b) {
        return a + b;
    }

    interface IntFunction {
        int apply(int b);
    }

    public static void main(String[] args) {
        IntFunction add5 = (b) -> add(5, b);
        System.out.println(add5.apply(3));
    }
}
