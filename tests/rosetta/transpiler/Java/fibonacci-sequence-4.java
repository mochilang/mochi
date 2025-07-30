public class Main {

    static int[] fib(int n) {
        int a = 0;
        int b = 1;
        int[] res = new int[]{};
        int i = 0;
        while (i < n) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(a)).toArray();
            int tmp = a + b;
            a = b;
            i = i + 1;
            b = tmp;
        }
        return res;
    }

    static void main() {
        int[] seq = fib(10);
        for (int v : seq) {
            System.out.println(String.valueOf(v));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
