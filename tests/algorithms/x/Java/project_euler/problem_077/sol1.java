public class Main {
    static int NUM_PRIMES;
    static int[] primes_1;
    static java.util.Map<Integer,int[]> partition_cache = null;
    static int result;

    static int[] generate_primes(int limit) {
        boolean[] is_prime = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i <= limit) {
            is_prime = ((boolean[])(appendBool(is_prime, true)));
            i = i + 1;
        }
is_prime[0] = false;
is_prime[1] = false;
        i = 2;
        while (i * i <= limit) {
            if (((Boolean)(is_prime[i]))) {
                int j = i * i;
                while (j <= limit) {
is_prime[j] = false;
                    j = j + i;
                }
            }
            i = i + 1;
        }
        int[] primes = ((int[])(new int[]{}));
        i = 2;
        while (i <= limit) {
            if (((Boolean)(is_prime[i]))) {
                primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        return primes;
    }

    static boolean contains(int[] xs, int value) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (xs[i_1] == value) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static int[] partition(int n) {
        if (n < 0) {
            return new int[]{};
        }
        if (n == 0) {
            return new int[]{1};
        }
        if (((Boolean)(partition_cache.containsKey(n)))) {
            return ((int[])(partition_cache).get(n));
        }
        int[] ret = ((int[])(new int[]{}));
        for (int prime : primes_1) {
            if (prime > n) {
                continue;
            }
            int[] subs = ((int[])(partition(n - prime)));
            for (int sub : subs) {
                int prod = sub * prime;
                if (!(Boolean)contains(((int[])(ret)), prod)) {
                    ret = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ret), java.util.stream.IntStream.of(prod)).toArray()));
                }
            }
        }
partition_cache.put(n, ((int[])(ret)));
        return ret;
    }

    static int solution(int threshold) {
        int number_to_partition = 1;
        while (number_to_partition < NUM_PRIMES) {
            int[] parts = ((int[])(partition(number_to_partition)));
            if (parts.length > threshold) {
                return number_to_partition;
            }
            number_to_partition = number_to_partition + 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        NUM_PRIMES = 100;
        primes_1 = ((int[])(generate_primes(NUM_PRIMES)));
        partition_cache = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
        result = solution(5000);
        System.out.println("solution() = " + _p(result));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
