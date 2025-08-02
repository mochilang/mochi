public class Main {

    static int nextPrime(int[] primes, int start) {
        int n = start;
        while (true) {
            boolean isP = true;
            int i = 0;
            while (i < primes.length) {
                int p = primes[i];
                if (p * p > n) {
                    break;
                }
                if (Math.floorMod(n, p) == 0) {
                    isP = false;
                    break;
                }
                i = i + 1;
            }
            if (isP) {
                return n;
            }
            n = n + 2;
        }
    }

    static void main() {
        int[] primes = new int[]{2};
        int cand = 3;
        while (primes.length < 10000) {
            cand = nextPrime(primes, cand);
            primes = java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(cand)).toArray();
            cand = cand + 2;
        }
        String line = "First twenty:";
        int i_1 = 0;
        while (i_1 < 20) {
            line = line + " " + (String)(_p(_geti(primes, i_1)));
            i_1 = i_1 + 1;
        }
        System.out.println(line);
        int idx = 0;
        while (primes[idx] <= 100) {
            idx = idx + 1;
        }
        line = "Between 100 and 150: " + (String)(_p(_geti(primes, idx)));
        idx = idx + 1;
        while (primes[idx] < 150) {
            line = line + " " + (String)(_p(_geti(primes, idx)));
            idx = idx + 1;
        }
        System.out.println(line);
        while (primes[idx] <= 7700) {
            idx = idx + 1;
        }
        int count = 0;
        while (primes[idx] < 8000) {
            count = count + 1;
            idx = idx + 1;
        }
        System.out.println("Number beween 7,700 and 8,000: " + (String)(_p(count)));
        System.out.println("10,000th prime: " + (String)(_p(_geti(primes, 9999))));
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
