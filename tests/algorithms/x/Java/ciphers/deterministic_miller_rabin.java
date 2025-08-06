public class Main {

    static int mod_pow(int base, int exp, int mod) {
        int result = 1;
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), mod);
            }
            b = Math.floorMod((b * b), mod);
            e = e / 2;
        }
        return result;
    }

    static boolean miller_rabin(int n, boolean allow_probable) {
        if (n == 2) {
            return true;
        }
        if (n < 2 || Math.floorMod(n, 2) == 0) {
            return false;
        }
        if (n > 5) {
            int last = Math.floorMod(n, 10);
            if (!(last == 1 || last == 3 || last == 7 || last == 9)) {
                return false;
            }
        }
        int limit = (int)3825123056546413051L;
        if (n > limit && (!(Boolean)allow_probable)) {
            throw new RuntimeException(String.valueOf("Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test."));
        }
        int[] bounds = ((int[])(new int[]{2047, 1373653, 25326001, (int)3215031751L, (int)2152302898747L, (int)3474749660383L, (int)341550071728321L, limit}));
        int[] primes = ((int[])(new int[]{2, 3, 5, 7, 11, 13, 17, 19}));
        int i = 0;
        int plist_len = primes.length;
        while (i < bounds.length) {
            if (n < bounds[i]) {
                plist_len = i + 1;
                i = bounds.length;
            } else {
                i = i + 1;
            }
        }
        int d = n - 1;
        int s = 0;
        while (Math.floorMod(d, 2) == 0) {
            d = d / 2;
            s = s + 1;
        }
        int j = 0;
        while (j < plist_len) {
            int prime = primes[j];
            int x = mod_pow(prime, d, n);
            boolean pr = false;
            if (x == 1 || x == n - 1) {
                pr = true;
            } else {
                int r = 1;
                while (r < s && (!pr)) {
                    x = Math.floorMod((x * x), n);
                    if (x == n - 1) {
                        pr = true;
                    }
                    r = r + 1;
                }
            }
            if (!pr) {
                return false;
            }
            j = j + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        System.out.println(_p(miller_rabin(561, false)));
        System.out.println(_p(miller_rabin(563, false)));
        System.out.println(_p(miller_rabin(838201, false)));
        System.out.println(_p(miller_rabin(838207, false)));
        System.out.println(_p(miller_rabin(17316001, false)));
        System.out.println(_p(miller_rabin(17316017, false)));
        System.out.println(_p(miller_rabin((int)3078386641L, false)));
        System.out.println(_p(miller_rabin((int)3078386653L, false)));
        System.out.println(_p(miller_rabin((int)1713045574801L, false)));
        System.out.println(_p(miller_rabin((int)1713045574819L, false)));
        System.out.println(_p(miller_rabin((int)2779799728307L, false)));
        System.out.println(_p(miller_rabin((int)2779799728327L, false)));
        System.out.println(_p(miller_rabin((int)113850023909441L, false)));
        System.out.println(_p(miller_rabin((int)113850023909527L, false)));
        System.out.println(_p(miller_rabin((int)1275041018848804351L, false)));
        System.out.println(_p(miller_rabin((int)1275041018848804391L, false)));
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
