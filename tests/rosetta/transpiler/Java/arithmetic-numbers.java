public class Main {

    static int[] sieve(int limit) {
        int[] spf = new int[]{};
        int i = 0;
        while (i <= limit) {
            spf = java.util.stream.IntStream.concat(java.util.Arrays.stream(spf), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        i = 2;
        while (i <= limit) {
            if (spf[i] == 0) {
spf[i] = i;
                if (i * i <= limit) {
                    int j = i * i;
                    while (j <= limit) {
                        if (spf[j] == 0) {
spf[j] = i;
                        }
                        j = j + i;
                    }
                }
            }
            i = i + 1;
        }
        return spf;
    }

    static int[] primesFrom(int[] spf, int limit) {
        int[] primes = new int[]{};
        int i = 3;
        while (i <= limit) {
            if (spf[i] == i) {
                primes = java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(i)).toArray();
            }
            i = i + 1;
        }
        return primes;
    }

    static String pad3(int n) {
        String s = String.valueOf(n);
        while (s.length() < 3) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static String commatize(int n) {
        String s = String.valueOf(n);
        String out = "";
        int i = s.length() - 1;
        int c = 0;
        while (i >= 0) {
            out = String.valueOf(s.substring(i, i + 1) + out);
            c = c + 1;
            if (c % 3 == 0 && i > 0) {
                out = String.valueOf("," + out);
            }
            i = i - 1;
        }
        return out;
    }

    static int primeCount(int[] primes, int last, int[] spf) {
        int lo = 0;
        int hi = primes.length;
        while (lo < hi) {
            int mid = ((Number)(((lo + hi) / 2))).intValue();
            if (primes[mid] < last) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        int count = lo + 1;
        if (spf[last] != last) {
            count = count - 1;
        }
        return count;
    }

    static int[] arithmeticNumbers(int limit, int[] spf) {
        int[] arr = new int[]{1};
        int n = 3;
        while (arr.length < limit) {
            if (spf[n] == n) {
                arr = java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(n)).toArray();
            } else {
                int x = n;
                int sigma = 1;
                int tau = 1;
                while (x > 1) {
                    int p = spf[x];
                    if (p == 0) {
                        p = x;
                    }
                    int cnt = 0;
                    int power = p;
                    int sum = 1;
                    while (x % p == 0) {
                        x = x / p;
                        cnt = cnt + 1;
                        sum = sum + power;
                        power = power * p;
                    }
                    sigma = sigma * sum;
                    tau = tau * (cnt + 1);
                }
                if (sigma % tau == 0) {
                    arr = java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(n)).toArray();
                }
            }
            n = n + 1;
        }
        return arr;
    }

    static void main() {
        int limit = 1228663;
        int[] spf = sieve(limit);
        int[] primes = primesFrom(spf, limit);
        int[] arr = arithmeticNumbers(1000000, spf);
        System.out.println("The first 100 arithmetic numbers are:");
        int i = 0;
        while (i < 100) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = String.valueOf(line + String.valueOf(pad3(arr[i + j])));
                if (j < 9) {
                    line = String.valueOf(line + " ");
                }
                j = j + 1;
            }
            System.out.println(line);
            i = i + 10;
        }
        for (int x : new int[]{1000, 10000, 100000, 1000000}) {
            int last = arr[x - 1];
            String lastc = String.valueOf(commatize(last));
            System.out.println(String.valueOf(String.valueOf("\nThe " + String.valueOf(commatize(x))) + "th arithmetic number is: ") + lastc);
            int pc = primeCount(primes, last, spf);
            int comp = x - pc - 1;
            System.out.println(String.valueOf(String.valueOf("The count of such numbers <= " + lastc) + " which are composite is " + commatize(comp)) + ".");
        }
    }
    public static void main(String[] args) {
        main();
    }
}
