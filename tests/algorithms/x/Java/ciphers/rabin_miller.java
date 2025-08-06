public class Main {
    static int p_1;

    static int int_pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int pow_mod(int base, int exp, int mod) {
        int result_1 = 1;
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result_1 = Math.floorMod((result_1 * b), mod);
            }
            e = e / 2;
            b = Math.floorMod((b * b), mod);
        }
        return result_1;
    }

    static int rand_range(int low, int high) {
        return (Math.floorMod(_now(), (high - low))) + low;
    }

    static boolean rabin_miller(int num) {
        int s = num - 1;
        int t = 0;
        while (Math.floorMod(s, 2) == 0) {
            s = s / 2;
            t = t + 1;
        }
        int k = 0;
        while (k < 5) {
            int a = rand_range(2, num - 1);
            int v = pow_mod(a, s, num);
            if (v != 1) {
                int i_1 = 0;
                while (v != (num - 1)) {
                    if (i_1 == t - 1) {
                        return false;
                    }
                    i_1 = i_1 + 1;
                    v = Math.floorMod((v * v), num);
                }
            }
            k = k + 1;
        }
        return true;
    }

    static boolean is_prime_low_num(int num) {
        if (num < 2) {
            return false;
        }
        int[] low_primes = ((int[])(new int[]{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}));
        if (((Boolean)(java.util.Arrays.stream(low_primes).anyMatch((x) -> ((Number)(x)).intValue() == num)))) {
            return true;
        }
        int i_2 = 0;
        while (i_2 < low_primes.length) {
            int p = low_primes[i_2];
            if (Math.floorMod(num, p) == 0) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return rabin_miller(num);
    }

    static int generate_large_prime(int keysize) {
        int start = int_pow(2, keysize - 1);
        int end = int_pow(2, keysize);
        while (true) {
            int num = rand_range(start, end);
            if (((Boolean)(is_prime_low_num(num)))) {
                return num;
            }
        }
    }
    public static void main(String[] args) {
        p_1 = generate_large_prime(16);
        System.out.println("Prime number: " + _p(p_1));
        System.out.println("is_prime_low_num: " + _p(is_prime_low_num(p_1)));
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
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
