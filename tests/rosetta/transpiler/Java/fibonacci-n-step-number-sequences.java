public class Main {

    static String show(int[] xs) {
        String s = "";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        return s;
    }

    static int[] gen(int[] init, int n) {
        int[] b = init;
        int[] res = new int[]{};
        int sum = 0;
        for (int x : b) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(x)).toArray();
            sum = sum + x;
        }
        while (res.length < n) {
            int next = sum;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(next)).toArray();
            sum = sum + next - b[0];
            b = java.util.stream.IntStream.concat(java.util.Arrays.stream(java.util.Arrays.copyOfRange(b, 1, b.length)), java.util.stream.IntStream.of(next)).toArray();
        }
        return res;
    }

    static void main() {
        int n = 10;
        System.out.println(" Fibonacci: " + String.valueOf(show(gen(new int[]{1, 1}, n))));
        System.out.println("Tribonacci: " + String.valueOf(show(gen(new int[]{1, 1, 2}, n))));
        System.out.println("Tetranacci: " + String.valueOf(show(gen(new int[]{1, 1, 2, 4}, n))));
        System.out.println("     Lucas: " + String.valueOf(show(gen(new int[]{2, 1}, n))));
    }
    public static void main(String[] args) {
        main();
    }
}
