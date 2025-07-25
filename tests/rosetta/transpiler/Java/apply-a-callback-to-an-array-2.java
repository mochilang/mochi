public class Main {

    static void each(int[] xs, java.util.function.Consumer<Integer> f) {
        for (var x : xs) {
            f.accept(x);
        }
    }

    static int[] Map(int[] xs, java.util.function.Function<Integer,Integer> f) {
        int[] r = new int[]{};
        for (var x : xs) {
            r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(f.apply(x))).toArray();
        }
        return r;
    }

    static void main() {
        int[] s = new int[]{1, 2, 3, 4, 5};
        each(s, (i) -> System.out.println(String.valueOf(((Number)(i)).intValue() * ((Number)(i)).intValue())));
        System.out.println(String.valueOf(Map(s, (i) -> ((Number)(i)).intValue() * ((Number)(i)).intValue())));
    }
    public static void main(String[] args) {
        main();
    }
}
