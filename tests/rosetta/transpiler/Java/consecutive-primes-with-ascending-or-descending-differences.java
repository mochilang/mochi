public class Main {
    static int LIMIT = 999999;
    static int[] primes = primesUpTo(LIMIT);

    static int[] primesUpTo(int n) {
        boolean[] sieve = new boolean[]{};
        int i = 0;
        while (i <= n) {
            sieve = appendBool(sieve, true);
            i = i + 1;
        }
        int p = 2;
        while (p * p <= n) {
            if (sieve[p]) {
                int m = p * p;
                while (m <= n) {
sieve[m] = false;
                    m = m + p;
                }
            }
            p = p + 1;
        }
        int[] res = new int[]{};
        int x = 2;
        while (x <= n) {
            if (sieve[x]) {
                res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(x)).toArray();
            }
            x = x + 1;
        }
        return res;
    }

    static void longestSeq(String dir) {
        int pd = 0;
        int[][] longSeqs = new int[][]{new int[]{2}};
        int[] currSeq = new int[]{2};
        int i = 1;
        while (i < primes.length) {
            int d = primes[i] - primes[i - 1];
            if (((dir.equals("ascending")) && d <= pd) || ((dir.equals("descending")) && d >= pd)) {
                if (currSeq.length > longSeqs[0].length) {
                    longSeqs = new int[][]{currSeq};
                } else                 if (currSeq.length == longSeqs[0].length) {
                    longSeqs = appendObj(longSeqs, currSeq);
                }
                currSeq = new int[]{primes[i - 1], primes[i]};
            } else {
                currSeq = java.util.stream.IntStream.concat(java.util.Arrays.stream(currSeq), java.util.stream.IntStream.of(primes[i])).toArray();
            }
            pd = d;
            i = i + 1;
        }
        if (currSeq.length > longSeqs[0].length) {
            longSeqs = new int[][]{currSeq};
        } else         if (currSeq.length == longSeqs[0].length) {
            longSeqs = appendObj(longSeqs, currSeq);
        }
        System.out.println("Longest run(s) of primes with " + dir + " differences is " + String.valueOf(longSeqs[0].length) + " :");
        for (int[] ls : longSeqs) {
            int[] diffs = new int[]{};
            int j = 1;
            while (j < ls.length) {
                diffs = java.util.stream.IntStream.concat(java.util.Arrays.stream(diffs), java.util.stream.IntStream.of(ls[j] - ls[j - 1])).toArray();
                j = j + 1;
            }
            int k = 0;
            while (k < ls.length - 1) {
                System.out.println(String.valueOf(ls[k]) + " (" + String.valueOf(diffs[k]) + ") " + " " + String.valueOf(false ? "True" : "False"));
                k = k + 1;
            }
            System.out.println(String.valueOf(ls[ls.length - 1]));
        }
        System.out.println("");
    }

    static void main() {
        System.out.println("For primes < 1 million:\n");
        for (String dir : new String[]{"ascending", "descending"}) {
            longestSeq(dir);
        }
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
