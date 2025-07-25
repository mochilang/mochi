public class Main {
    static int[] asc = new int[]{};

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (n % 2 == 0) {
            return n == 2;
        }
        if (n % 3 == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) {
                return false;
            }
            d = d + 2;
            if (n % d == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static void gen(int first, int cand, int digits) {
        if (digits == 0) {
            if (isPrime(cand)) {
                asc = java.util.stream.IntStream.concat(java.util.Arrays.stream(asc), java.util.Arrays.stream(new int[]{cand})).toArray();
            }
            return;
        }
        int i = first;
        while (i < 10) {
            gen(i + 1, cand * 10 + i, digits - 1);
            i = i + 1;
        }
    }

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static void main() {
        int digits = 1;
        while (digits < 10) {
            gen(1, 0, digits);
            digits = digits + 1;
        }
        System.out.println(String.valueOf("There are " + String.valueOf(asc.length)) + " ascending primes, namely:");
        int i = 0;
        String line = "";
        while (i < asc.length) {
            line = String.valueOf(String.valueOf(line + String.valueOf(pad(asc[i], 8))) + " ");
            if ((i + 1) % 10 == 0) {
                System.out.println(line.substring(0, line.length() - 1));
                line = "";
            }
            i = i + 1;
        }
        if (line.length() > 0) {
            System.out.println(line.substring(0, line.length() - 1));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
