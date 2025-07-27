public class Main {

    static int pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static boolean isDisarium(int n) {
        int[] digits = new int[]{};
        int x = n;
        if (x == 0) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(0)).toArray();
        }
        while (x > 0) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(Math.floorMod(x, 10))).toArray();
            x = ((Number)((x / 10))).intValue();
        }
        int sum = 0;
        int pos = 1;
        int i = digits.length - 1;
        while (i >= 0) {
            sum = sum + pow(digits[i], pos);
            pos = pos + 1;
            i = i - 1;
        }
        return sum == n;
    }

    static void main() {
        int count = 0;
        int n = 0;
        while (count < 19 && n < 3000000) {
            if (isDisarium(n)) {
                System.out.println(String.valueOf(n));
                count = count + 1;
            }
            n = n + 1;
        }
        System.out.println("\nFound the first " + String.valueOf(count) + " Disarium numbers.");
    }
    public static void main(String[] args) {
        main();
    }
}
