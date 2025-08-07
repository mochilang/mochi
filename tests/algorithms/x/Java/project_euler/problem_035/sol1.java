public class Main {
    static int LIMIT;
    static boolean[] sieve = new boolean[0];
    static int i = 0;
    static int p = 0;

    static boolean is_prime(int n) {
        return sieve[n];
    }

    static boolean contains_an_even_digit(int n) {
        String s = _p(n);
        int idx = 0;
        while (idx < _runeLen(s)) {
            String c = s.substring(idx, idx+1);
            if ((c.equals("0")) || (c.equals("2")) || (c.equals("4")) || (c.equals("6")) || (c.equals("8"))) {
                return true;
            }
            idx = idx + 1;
        }
        return false;
    }

    static int parse_int(String s) {
        int value = 0;
        int k = 0;
        while (k < _runeLen(s)) {
            String ch = s.substring(k, k+1);
            value = value * 10 + (Integer.parseInt(ch));
            k = k + 1;
        }
        return value;
    }

    static int[] find_circular_primes(int limit) {
        int[] result = ((int[])(new int[]{2}));
        int num = 3;
        while (num <= limit) {
            if (((Boolean)(is_prime(num))) && (contains_an_even_digit(num) == false)) {
                String s_1 = _p(num);
                boolean all_prime = true;
                int j_1 = 0;
                while (j_1 < _runeLen(s_1)) {
                    String rotated_str = _substr(s_1, j_1, _runeLen(s_1)) + _substr(s_1, 0, j_1);
                    int rotated = parse_int(rotated_str);
                    if (!(Boolean)is_prime(rotated)) {
                        all_prime = false;
                        break;
                    }
                    j_1 = j_1 + 1;
                }
                if (all_prime) {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(num)).toArray()));
                }
            }
            num = num + 2;
        }
        return result;
    }

    static int solution() {
        return find_circular_primes(LIMIT).length;
    }
    public static void main(String[] args) {
        LIMIT = 10000;
        sieve = ((boolean[])(new boolean[]{}));
        i = 0;
        while (i <= LIMIT) {
            sieve = ((boolean[])(appendBool(sieve, true)));
            i = i + 1;
        }
        p = 2;
        while (p * p <= LIMIT) {
            if (((Boolean)(sieve[p]))) {
                int j = p * p;
                while (j <= LIMIT) {
sieve[j] = false;
                    j = j + p;
                }
            }
            p = p + 1;
        }
        System.out.println("len(find_circular_primes()) = " + _p(solution()));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
