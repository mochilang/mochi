public class Main {

    static int isqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static int solution(int n) {
        boolean[] sieve = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i <= n) {
            sieve = ((boolean[])(appendBool(sieve, false)));
            i = i + 1;
        }
sieve[0] = true;
sieve[1] = true;
        int limit = isqrt(n);
        int p = 2;
        while (p <= limit) {
            if (!(Boolean)sieve[p]) {
                int j = p * p;
                while (j <= n) {
sieve[j] = true;
                    j = j + p;
                }
            }
            p = p + 1;
        }
        int sum = 0;
        int k = 2;
        while (k < n) {
            if (!(Boolean)sieve[k]) {
                sum = sum + k;
            }
            k = k + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(20000)));
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
