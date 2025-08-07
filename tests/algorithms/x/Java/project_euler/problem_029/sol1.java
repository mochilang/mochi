public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int int_pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int solution(int n) {
        int[] powers = ((int[])(new int[]{}));
        int limit = n + 1;
        for (int a = 2; a < limit; a++) {
            for (int b = 2; b < limit; b++) {
                int p = int_pow(a, b);
                if (!(Boolean)(java.util.Arrays.stream(powers).anyMatch((x) -> ((Number)(x)).intValue() == p))) {
                    powers = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(powers), java.util.stream.IntStream.of(p)).toArray()));
                }
            }
        }
        return powers.length;
    }

    static void main() {
        int n = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println("Number of terms " + " " + String.valueOf(solution(n)));
    }
    public static void main(String[] args) {
        main();
    }
}
