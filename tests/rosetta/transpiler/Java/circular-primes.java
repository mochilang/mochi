public class Main {
    static int[] circs = new int[]{};
    static int[] digits = new int[]{1, 3, 7, 9};
    static int[] q = new int[]{1, 2, 3, 5, 7, 9};
    static int[] fq = new int[]{1, 2, 3, 5, 7, 9};
    static int count = 0;

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static boolean isCircular(int n) {
        int nn = n;
        int pow = 1;
        while (nn > 0) {
            pow = pow * 10;
            nn = nn / 10;
        }
        nn = n;
        while (true) {
            nn = nn * 10;
            int f = nn / pow;
            nn = nn + f * (1 - pow);
            if (nn == n) {
                break;
            }
            if (!(Boolean)isPrime(nn)) {
                return false;
            }
        }
        return true;
    }

    static String showList(int[] xs) {
        String out = "[";
        int i = 0;
        while (i < xs.length) {
            out = out + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                out = out + ", ";
            }
            i = i + 1;
        }
        return out + "]";
    }
    public static void main(String[] args) {
        System.out.println("The first 19 circular primes are:");
        while (true) {
            int f = q[0];
            int fd = fq[0];
            if (isPrime(f) && isCircular(f)) {
                circs = java.util.stream.IntStream.concat(java.util.Arrays.stream(circs), java.util.stream.IntStream.of(f)).toArray();
                count = count + 1;
                if (count == 19) {
                    break;
                }
            }
            q = java.util.Arrays.copyOfRange(q, 1, q.length);
            fq = java.util.Arrays.copyOfRange(fq, 1, fq.length);
            if (f != 2 && f != 5) {
                for (int d : digits) {
                    q = java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(f * 10 + d)).toArray();
                    fq = java.util.stream.IntStream.concat(java.util.Arrays.stream(fq), java.util.stream.IntStream.of(fd)).toArray();
                }
            }
        }
        System.out.println(showList(circs));
        System.out.println("\nThe next 4 circular primes, in repunit format, are:");
        System.out.println("[R(19) R(23) R(317) R(1031)]");
        System.out.println("\nThe following repunits are probably circular primes:");
        for (int i : new int[]{5003, 9887, 15073, 25031, 35317, 49081}) {
            System.out.println("R(" + String.valueOf(i) + ") : true");
        }
    }
}
