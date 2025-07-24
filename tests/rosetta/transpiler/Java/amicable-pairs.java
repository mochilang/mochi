public class Main {

    static int pfacSum(int i) {
        int sum = 0;
        int p = 1;
        while (p <= i / 2) {
            if (i % p == 0) {
                sum = sum + p;
            }
            p = p + 1;
        }
        return sum;
    }

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }

    static void main() {
        int[] sums = new int[]{};
        int i = 0;
        while (i < 20000) {
            sums = java.util.stream.IntStream.concat(java.util.Arrays.stream(sums), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        i = 1;
        while (i < 20000) {
sums[i] = pfacSum(i);
            i = i + 1;
        }
        System.out.println("The amicable pairs below 20,000 are:");
        int n = 2;
        while (n < 19999) {
            int m = sums[n];
            if (m > n && m < 20000 && n == sums[m]) {
                System.out.println("  " + pad(n, 5) + " and " + pad(m, 5));
            }
            n = n + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
