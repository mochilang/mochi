public class Main {

    static boolean kPrime(int n, int k) {
        int nf = 0;
        int i = 2;
        while (i <= n) {
            while (n % i == 0) {
                if (nf == k) {
                    return false;
                }
                nf = nf + 1;
                n = n / i;
            }
            i = i + 1;
        }
        return nf == k;
    }

    static int[] gen(int k, int count) {
        int[] r = new int[]{};
        int n = 2;
        while (r.length < count) {
            if (kPrime(n, k)) {
                r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(n)).toArray();
            }
            n = n + 1;
        }
        return r;
    }

    static void main() {
        int k = 1;
        while (k <= 5) {
            System.out.println(String.valueOf(k) + " " + String.valueOf(gen(k, 10)));
            k = k + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
