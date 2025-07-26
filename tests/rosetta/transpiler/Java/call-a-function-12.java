public class Main {

    static java.util.function.Function<Integer,Integer> mkAdd(int a) {
        return (b) -> a + ((Number)(b)).intValue();
    }

    static int mysum(int x, int y) {
        return x + y;
    }

    static java.util.function.Function<Integer,Integer> partialSum(int x) {
        return (y) -> mysum(x, ((Number)(y)).intValue());
    }

    static void main() {
        java.util.function.Function<Integer,Integer> add2 = mkAdd(2);
        java.util.function.Function<Integer,Integer> add3 = mkAdd(3);
        System.out.println(String.valueOf(add2.apply(5)) + " " + String.valueOf(add3.apply(6)));
        java.util.function.Function<Integer,Integer> partial = partialSum(13);
        System.out.println(String.valueOf(partial.apply(5)));
    }
    public static void main(String[] args) {
        main();
    }
}
